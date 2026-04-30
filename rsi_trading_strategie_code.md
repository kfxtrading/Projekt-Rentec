# RSI-basierte Trading-Strategie in R

Diese Markdown-Datei enthält den von dir bereitgestellten R-Code zur RSI-basierten Trading-Strategie.

```r
# ===================================================================
# 1. SETUP: Pakete laden und Parameter definieren
# ===================================================================

# Benötigte Pakete
if (!require(quantmod)) install.packages("quantmod")
if (!require(TTR)) install.packages("TTR")
if (!require(scales)) install.packages("scales")
if (!require(foreach)) install.packages("foreach")
if (!require(doParallel)) install.packages("doParallel")
if (!require(dlm)) install.packages("dlm")
if (!require(progress)) install.packages("progress")
if (!require(xgboost)) install.packages("xgboost")
if (!require(rugarch)) install.packages("rugarch")
if (!require(rmarkdown)) install.packages("rmarkdown")
if (!require(highcharter)) install.packages("highcharter")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(htmltools)) install.packages("htmltools")
if (!require(rstudioapi)) install.packages("rstudioapi")

library(quantmod); library(TTR); library(scales); library(foreach); library(doParallel);
library(dlm); library(progress); library(xgboost); library(rugarch); library(rmarkdown);
library(highcharter); library(PerformanceAnalytics); library(htmltools); library(rstudioapi)

# --- Basisparameter ---
initial_capital <- 1000
holding_period <- 10
data_start_date <- "1995-01-01"     # Lade Daten ab hier für den Warm-up
backtest_start_date <- "1999-01-01" # Starte die Simulation exakt ab hier
leverage <- 200
min_lot_pct <- 0.02
max_lot_pct <- 0.30

# --- Ordner & Dateinamen für Speicherung ---
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- paste0("run_", run_timestamp)
dir.create(output_dir)
checkpoint_file <- file.path(output_dir, "optimization_checkpoint.rds")

# --- Asset-Parameter ---
signal_symbol <- "EURUSD=X"
filter_symbol <- "GC=F"

# --- Optimierungs-Parameter ---
sma_period_range <- seq(50, 150, by = 5) 
rsi_period_range <- seq(10, 14, by = 2)
rsi_oversold_range <- seq(20, 30, by = 2)
rsi_overbought_range <- seq(70, 90, by = 2)
filter_fast_range <- seq(20, 50, by = 2)
filter_slow_range <- seq(60, 250, by = 5)
use_kalman_options <- c(TRUE, FALSE)


# ===================================================================
# 2. DATENLADUNG (lange Historie für Warm-up)
# ===================================================================
cat("Lade lange Datenhistorie ab", data_start_date, "für Indikator-Warm-up...\n")
signal_data_raw <- getSymbols(signal_symbol, src = "yahoo", from = data_start_date, auto.assign = FALSE)
signal_data_raw <- na.omit(signal_data_raw)

filter_data_raw <- getSymbols(filter_symbol, src = "yahoo", from = data_start_date, auto.assign = FALSE)
filter_data_raw <- na.omit(filter_data_raw)


# ===================================================================
# 3. HELFERFUNKTIONEN (Kalman & Datenaufbereitung)
# ===================================================================
apply_kalman_filter <- function(time_series) {
  model <- dlmModPoly(order = 1, dV = 0.8, dW = 0.1)
  smoothed <- dlmSmooth(dlmFilter(time_series, model))
  return(xts(smoothed$s[-1], order.by = index(time_series)))
}

prepare_backtest_data <- function(s_data, f_data, params, b_start_date) {
  s_data$sma <- SMA(Cl(s_data), n = params$sma_p)
  raw_rsi <- RSI(Cl(s_data), n = params$rsi_p)
  s_data$rsi <- if(params$use_kalman) apply_kalman_filter(raw_rsi) else raw_rsi
  f_data$fast_ma <- SMA(Cl(f_data), n = params$f_fast)
  f_data$slow_ma <- SMA(Cl(f_data), n = params$f_slow)
  all_data <- merge(s_data, f_data[, c("fast_ma", "slow_ma")], join = "inner")
  all_data <- na.omit(all_data)
  return(all_data[paste0(b_start_date, "/")])
}


# ===================================================================
# 4. BACKTEST-FUNKTIONEN (schnell & detailliert)
# ===================================================================
run_strategy_backtest <- function(prepared_data, params) {
  if (nrow(prepared_data) < (holding_period + 1)) return(initial_capital)
  current_capital <- initial_capital; position_open <- FALSE; entry_bar <- 0
  for (i in 2:nrow(prepared_data)) {
    if (position_open && (i >= entry_bar + holding_period)) { position_open <- FALSE }
    if (!position_open) {
      filter_is_bullish <- prepared_data$fast_ma[i] > prepared_data$slow_ma[i]
      filter_is_bearish <- prepared_data$fast_ma[i] < prepared_data$slow_ma[i]
      buy_signal <- Cl(prepared_data)[i] < prepared_data$sma[i] && prepared_data$rsi[i-1] < params$rsi_os && prepared_data$rsi[i] >= params$rsi_os
      sell_signal <- Cl(prepared_data)[i] > prepared_data$sma[i] && prepared_data$rsi[i-1] > params$rsi_ob && prepared_data$rsi[i] <= params$rsi_ob
      direction <- "None"; if(buy_signal && filter_is_bullish) direction <- "Long"; if(sell_signal && filter_is_bearish) direction <- "Short"
      if (direction != "None") {
        if (i + holding_period > nrow(prepared_data)) next
        exit_price <- as.numeric(Cl(prepared_data)[i + holding_period]); if (is.na(exit_price)) next
        return_pct <- (exit_price / as.numeric(Cl(prepared_data)[i])) - 1
        pnl <- current_capital * 0.20 * ifelse(direction == "Long", return_pct, -return_pct)
        current_capital <- current_capital + pnl; position_open <- TRUE; entry_bar <- i
      }
    }
  }
  return(current_capital)
}

run_detailed_backtest_phase1 <- function(prepared_data, params) {
  equity_curve <- xts(rep(initial_capital, nrow(prepared_data)), order.by = index(prepared_data))
  trades <- data.frame()
  current_capital <- initial_capital; position_open <- FALSE; entry_bar <- 0
  for (i in 2:nrow(prepared_data)) {
    equity_curve[i] <- current_capital # Update equity curve daily
    if (position_open && (i >= entry_bar + holding_period)) { position_open <- FALSE }
    if (!position_open) {
      filter_is_bullish <- prepared_data$fast_ma[i] > prepared_data$slow_ma[i]
      filter_is_bearish <- prepared_data$fast_ma[i] < prepared_data$slow_ma[i]
      buy_signal <- Cl(prepared_data)[i] < prepared_data$sma[i] && prepared_data$rsi[i-1] < params$rsi_os && prepared_data$rsi[i] >= params$rsi_os
      sell_signal <- Cl(prepared_data)[i] > prepared_data$sma[i] && prepared_data$rsi[i-1] > params$rsi_ob && prepared_data$rsi[i] <= params$rsi_ob
      direction <- "None"; if(buy_signal && filter_is_bullish) direction <- "Long"; if(sell_signal && filter_is_bearish) direction <- "Short"
      if (direction != "None") {
        if (i + holding_period > nrow(prepared_data)) next
        entry_price <- as.numeric(Cl(prepared_data)[i])
        exit_price <- as.numeric(Cl(prepared_data)[i + holding_period])
        return_pct <- (exit_price / entry_price) - 1
        pnl <- current_capital * 0.20 * ifelse(direction == "Long", return_pct, -return_pct)
        current_capital <- current_capital + pnl
        position_open <- TRUE; entry_bar <- i
        trades <- rbind(trades, data.frame(EntryDate=index(prepared_data)[i], Direction=direction, PnL=pnl))
      }
    }
  }
  equity_curve[nrow(equity_curve)] <- current_capital
  return(list(trades = trades, equity_curve = equity_curve))
}


# ===================================================================
# 5. PHASE 1: OPTIMIERUNG
# ===================================================================
cat("Phase 1: Starte parallele Optimierung...\n")
results_so_far <- if (file.exists(checkpoint_file)) readRDS(checkpoint_file) else data.frame()
if(nrow(results_so_far) > 0) cat(nrow(results_so_far), "Kombinationen aus Checkpoint geladen.\n")
param_grid <- expand.grid(sma_p = sma_period_range, rsi_p = rsi_period_range, rsi_os = rsi_oversold_range, 
                          rsi_ob = rsi_overbought_range, f_fast = filter_fast_range, f_slow = filter_slow_range,
                          use_kalman = use_kalman_options)
param_grid <- subset(param_grid, f_fast < f_slow)
if(nrow(results_so_far) > 0) {
  param_grid$id <- apply(param_grid, 1, paste, collapse = "-")
  results_so_far$id <- apply(results_so_far[,1:7], 1, paste, collapse = "-")
  param_grid_to_run <- param_grid[!param_grid$id %in% results_so_far$id, ]
  param_grid_to_run$id <- NULL; results_so_far$id <- NULL
} else { param_grid_to_run <- param_grid }
chunk_size <- 5000; num_chunks <- ceiling(nrow(param_grid_to_run) / chunk_size)
pb <- progress_bar$new(format = "  Gesamtfortschritt [:bar] :percent ETA: :eta", total = num_chunks, width=80)
num_cores <- detectCores() - 1; my_cluster <- makeCluster(num_cores); registerDoParallel(my_cluster)
for (k in 1:num_chunks) {
  if(nrow(param_grid_to_run) == 0) break
  start_row <- 1; end_row <- min(chunk_size, nrow(param_grid_to_run)); current_chunk_params <- param_grid_to_run[start_row:end_row, ]
  cat(paste("\nVerarbeite Block", k, "von", num_chunks, "(", nrow(current_chunk_params), "Kombinationen)...\n"))
  new_results <- foreach(i = 1:nrow(current_chunk_params), .combine = 'rbind', .packages = c('quantmod', 'TTR', 'dlm'), .export = c("apply_kalman_filter", "prepare_backtest_data", "run_strategy_backtest", "backtest_start_date", "initial_capital", "holding_period")) %dopar% {
    params <- current_chunk_params[i, ]
    prepared_data <- prepare_backtest_data(signal_data_raw, filter_data_raw, params, backtest_start_date)
    final_equity <- run_strategy_backtest(prepared_data, params)
    cbind(current_chunk_params[i,], FinalEquity = final_equity)
  }
  results_so_far <- rbind(results_so_far, new_results); saveRDS(results_so_far, checkpoint_file)
  cat(paste("\nFortschritt gespeichert. Insgesamt", nrow(results_so_far), "Ergebnisse.\n"))
  param_grid_to_run <- param_grid_to_run[-(start_row:end_row), ]; pb$tick()
}
stopCluster(my_cluster)
optimization_results <- results_so_far
best_params <- optimization_results[which.max(optimization_results$FinalEquity), ]
if (file.exists(checkpoint_file)) file.remove(checkpoint_file)
cat("\nPhase 1 abgeschlossen. Beste Parameter gefunden.\n")


# ===================================================================
# 5.5 ZWISCHENBERICHT FÜR PHASE 1
# ===================================================================
cat("\n--- Erstelle Zwischenbericht für Phase 1 ---\n")
prepared_data_best <- prepare_backtest_data(signal_data_raw, filter_data_raw, best_params, backtest_start_date)
detailed_results <- run_detailed_backtest_phase1(prepared_data_best, best_params)
equity_curve_best <- detailed_results$equity_curve; returns_best <- na.omit(Return.calculate(equity_curve_best))
final_equity <- as.numeric(last(equity_curve_best)); total_return <- final_equity / initial_capital - 1
max_dd <- maxDrawdown(returns_best); sharpe_ratio <- SharpeRatio.annualized(returns_best, Rf = 0)

report_html <- tagList(
  tags$h2("Zwischenbericht: Beste Strategie aus Phase 1"), tags$hr(),
  tags$p(paste("Beste gefundene Architektur (Kalman-Filter):", best_params$use_kalman)),
  tags$p(paste("SMA:", best_params$sma_p, "| RSI:", best_params$rsi_p, "| OS/OB:", best_params$rsi_os, "/", best_params$rsi_ob)), tags$hr(),
  tags$h3("Performance Metriken"),
  tags$table(class = "table",
             tags$tr(tags$td("Endkapital:"), tags$td(dollar(final_equity))),
             tags$tr(tags$td("Gesamtrendite:"), tags$td(percent(total_return))),
             tags$tr(tags$td("Maximaler Drawdown:"), tags$td(percent(max_dd))),
             tags$tr(tags$td("Annualisierte Sharpe Ratio:"), tags$td(round(sharpe_ratio, 2)))
  ), tags$hr(),
  hchart(equity_curve_best, name = "Equity") %>% hc_title(text = "Equity-Kurve") %>% hc_add_theme(hc_theme_flat())
)
if(interactive() && "rstudioapi" %in% loadedNamespaces()) {
  html_file_path <- file.path(output_dir, "zwischenbericht_phase1.html")
  save_html(report_html, file = html_file_path)
  viewer(html_file_path)
  cat("Zwischenbericht wurde im Viewer-Fenster geöffnet und im Projektordner gespeichert.\n")
} else { print(report_html) }


# ===================================================================
# 6. PHASE 1.5: GARCH-VOLATILITÄTSPROGNOSE
# ===================================================================
cat("\nPhase 1.5: Erstelle rollierende GARCH-Prognose...\n")
returns_garch <- na.omit(Delt(Cl(signal_data_raw))); names(returns_garch) <- "Return"
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0)), distribution.model = "norm")
n <- nrow(returns_garch); forecasts <- numeric(n); lookback <- 500
for (i in (lookback + 1):n) {
  window_returns <- returns_garch[(i - lookback):(i - 1), ]; fit <- try(ugarchfit(spec, data = window_returns, solver = 'hybrid'), silent=T)
  if(inherits(fit, "try-error")) { forecasts[i] <- forecasts[i-1]; next }
  fore <- ugarchforecast(fit, n.ahead = 1); forecasts[i] <- sigma(fore)[1,1]
  if (i %% 100 == 0) cat("GARCH-Fortschritt:", round(i/n*100), "%\r")
}
garch_vol_forecast <- xts(forecasts, order.by = index(returns_garch)); names(garch_vol_forecast) <- "GARCH.Forecast"
cat("\nGARCH-Prognose abgeschlossen.\n")


# ===================================================================
# 7. PHASE 2: ML DATEN-VORBEREITUNG
# ===================================================================
cat("Phase 2: Starte Vorbereitung für Machine Learning...\n")
s_data <- signal_data_raw; f_data <- filter_data_raw
s_data$sma <- SMA(Cl(s_data), n = best_params$sma_p)
raw_rsi <- RSI(Cl(s_data), n = best_params$rsi_p)
s_data$rsi <- if(best_params$use_kalman) apply_kalman_filter(raw_rsi) else raw_rsi
f_data$fast_ma <- SMA(Cl(f_data), n = best_params$f_fast); f_data$slow_ma <- SMA(Cl(f_data), n = best_params$f_slow)
all_data <- merge(s_data, f_data[, c("fast_ma", "slow_ma")], garch_vol_forecast, join = "inner")
all_data$GARCH.Trend <- SMA(all_data$GARCH.Forecast, n=5)
all_data <- na.omit(all_data); all_data <- all_data[paste0(backtest_start_date, "/")]
ml_data_list <- list(); avg_garch_vol <- mean(all_data$GARCH.Forecast, na.rm=T)
for (i in 2:nrow(all_data)) {
  filter_bullish <- all_data$fast_ma[i] > all_data$slow_ma[i]; filter_bearish <- all_data$fast_ma[i] < all_data$slow_ma[i]
  buy_signal <- Cl(all_data)[i] < all_data$sma[i] && all_data$rsi[i-1] < best_params$rsi_os && all_data$rsi[i] >= best_params$rsi_os
  sell_signal <- Cl(all_data)[i] > all_data$sma[i] && all_data$rsi[i-1] > best_params$rsi_ob && all_data$rsi[i] <= best_params$rsi_ob
  if ((buy_signal && filter_bullish) || (sell_signal && filter_bearish)) {
    garch_feature <- as.numeric(all_data$GARCH.Forecast[i])
    sma_dist_feature <- abs(as.numeric(Cl(all_data)[i]) / as.numeric(all_data$sma[i]) - 1)
    vol_trend_feature <- garch_feature - as.numeric(all_data$GARCH.Trend[i])
    if(buy_signal) { rsi_strength_feature <- best_params$rsi_os - as.numeric(all_data$rsi[i])
    } else { rsi_strength_feature <- as.numeric(all_data$rsi[i]) - best_params$rsi_ob }
    optimal_lot <- min_lot_pct * (avg_garch_vol / garch_feature); optimal_lot <- max(min_lot_pct, min(max_lot_pct, optimal_lot))
    ml_data_list[[length(ml_data_list) + 1]] <- data.frame(GARCH_Forecast=garch_feature, SMA_Dist=sma_dist_feature,RSI_Strength=rsi_strength_feature, Vol_Trend=vol_trend_feature,OptimalLot=optimal_lot)
  }
}
ml_data <- do.call(rbind, ml_data_list); cat(nrow(ml_data), "Trainingsbeispiele generiert.\n")


# ===================================================================
# 8. PHASE 2: ML MODELLTRAINING
# ===================================================================
cat("Trainiere XGBoost-Modell...\n")
if(nrow(ml_data) > 20){
  train_size <- floor(0.75 * nrow(ml_data)); train_set <- ml_data[1:train_size, ]
  train_features <- as.matrix(train_set[, c("GARCH_Forecast", "SMA_Dist", "RSI_Strength", "Vol_Trend")])
  train_labels <- train_set$OptimalLot
  lot_size_model <- xgboost(data = train_features, label = train_labels, nrounds = 100, objective = "reg:squarederror", verbose = 0)
  cat("Modelltraining abgeschlossen.\n")
} else { lot_size_model <- NULL; cat("Nicht genügend Daten für ML-Modell.\n") }


# ===================================================================
# 9. PHASE 2: FINALER BACKTEST
# ===================================================================
cat("Starte finalen Backtest...\n")
trades <- data.frame(); current_capital <- initial_capital
for (i in 2:(nrow(all_data) - holding_period)) {
  if (current_capital <= 0) break
  filter_bullish <- all_data$fast_ma[i] > all_data$slow_ma[i]; filter_bearish <- all_data$fast_ma[i] < all_data$slow_ma[i]
  buy_signal <- Cl(all_data)[i] < all_data$sma[i] && all_data$rsi[i-1] < best_params$rsi_os && all_data$rsi[i] >= best_params$rsi_os
  sell_signal <- Cl(all_data)[i] > all_data$sma[i] && all_data$rsi[i-1] > best_params$rsi_ob && all_data$rsi[i] <= best_params$rsi_ob
  direction <- "None"; if(buy_signal && filter_bullish) direction <- "Long"; if(sell_signal && filter_bearish) direction <- "Short"
  if (direction != "None") {
    dynamic_lot_pct <- min_lot_pct
    if(!is.null(lot_size_model)){
      sma_dist_live <- abs(as.numeric(Cl(all_data)[i]) / as.numeric(all_data$sma[i]) - 1)
      vol_trend_live <- as.numeric(all_data$GARCH.Forecast[i]) - as.numeric(all_data$GARCH.Trend[i])
      if(direction == "Long") { rsi_strength_live <- best_params$rsi_os - as.numeric(all_data$rsi[i])
      } else { rsi_strength_live <- as.numeric(all_data$rsi[i]) - best_params$rsi_ob }
      features_for_prediction <- as.matrix(data.frame(GARCH_Forecast = as.numeric(all_data$GARCH.Forecast[i]), SMA_Dist = sma_dist_live, RSI_Strength = rsi_strength_live, Vol_Trend = vol_trend_live))
      predicted_lot <- predict(lot_size_model, features_for_prediction)
      dynamic_lot_pct <- max(min_lot_pct, min(max_lot_pct, predicted_lot))
    }
    entry_price <- as.numeric(Cl(all_data)[i]); exit_price <- as.numeric(Cl(all_data)[i + holding_period])
    return_pct <- (exit_price / entry_price) - 1
    margin_used <- current_capital * dynamic_lot_pct; position_value <- margin_used * leverage
    pnl <- position_value * ifelse(direction == "Long", return_pct, -return_pct)
    current_capital <- current_capital + pnl
    trades <- rbind(trades, data.frame(Date = index(all_data)[i], PnL = pnl, LotPct_as_Margin = dynamic_lot_pct * 100, Equity = current_capital))
  }
}


# ===================================================================
# 10. FINALE ERGEBNISANALYSE & SPEICHERN
# ===================================================================
cat("\n--- Finale Analyse und Speicherung ---\n")
# ... (Hier würde der finale Speicherprozess für alle Artefakte folgen) ...

if(nrow(trades) > 0) {
  final_equity <- last(trades$Equity); total_return_pct <- (final_equity / initial_capital - 1) * 100
  cat("Beste Basis-Parameter (inkl. Kalman):\n"); print(best_params)
  cat("\nStartkapital:", dollar(initial_capital), "\n"); cat("Endkapital:", dollar(final_equity), "\n")
  cat("Gesamtrendite:", round(total_return_pct, 2), "%\n\n")
  cat("Statistik der vom ML-Modell gewählten Margin (in %):\n")
  print(summary(trades$LotPct_as_Margin))
} else {
  cat("Keine Trades im finalen Backtest ausgeführt.\n")
}

cat("\n--- Alle Operationen abgeschlossen ---\n")
```
