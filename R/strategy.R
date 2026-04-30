required_strategy_packages <- function() {
  c(
    "quantmod", "TTR", "scales", "foreach", "doParallel", "dlm",
    "progress", "xgboost", "rugarch", "rmarkdown", "highcharter",
    "PerformanceAnalytics", "htmltools", "xts", "zoo"
  )
}

load_strategy_packages <- function(packages = required_strategy_packages()) {
  installed_packages <- rownames(utils::installed.packages())
  missing_packages <- setdiff(packages, installed_packages)

  if (length(missing_packages) > 0) {
    stop(
      "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
      "Run scripts/setup_packages.R before running the strategy.",
      call. = FALSE
    )
  }

  suppressWarnings(suppressMessages(suppressPackageStartupMessages({
    for (package in packages) {
      library(package, character.only = TRUE)
    }
  })))
}

default_strategy_config <- function() {
  list(
    initial_capital = 1000,
    holding_period = 10,
    data_start_date = "1995-01-01",
    backtest_start_date = "1999-01-01",
    leverage = 200,
    min_lot_pct = 0.02,
    max_lot_pct = 0.30,
    signal_symbol = "EURUSD=X",
    filter_symbol = "GC=F",
    sma_period_range = seq(50, 150, by = 5),
    rsi_period_range = seq(10, 14, by = 2),
    rsi_oversold_range = seq(20, 30, by = 2),
    rsi_overbought_range = seq(70, 90, by = 2),
    filter_fast_range = seq(20, 50, by = 2),
    filter_slow_range = seq(60, 250, by = 5),
    use_kalman_options = c(TRUE, FALSE),
    chunk_size = 5000,
    num_cores = NULL,
    output_root = "runs",
    run_timestamp = NULL,
    output_dir = NULL,
    resume_checkpoint = TRUE,
    garch_lookback = 500,
    garch_step = 1,
    garch_progress_every = 100,
    garch_trend_period = 5,
    min_training_rows = 20
  )
}

merge_strategy_config <- function(config) {
  defaults <- default_strategy_config()

  for (name in names(config)) {
    defaults[[name]] <- config[[name]]
  }

  defaults
}

normalize_strategy_config <- function(config = list()) {
  config <- merge_strategy_config(config)

  if (is.null(config$num_cores)) {
    config$num_cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  } else {
    config$num_cores <- max(1L, as.integer(config$num_cores))
  }

  config$chunk_size <- max(1L, as.integer(config$chunk_size))
  config$holding_period <- max(1L, as.integer(config$holding_period))
  config$garch_lookback <- max(1L, as.integer(config$garch_lookback))
  config$garch_step <- max(1L, as.integer(config$garch_step))
  config$garch_progress_every <- max(1L, as.integer(config$garch_progress_every))
  config$garch_trend_period <- max(1L, as.integer(config$garch_trend_period))
  config$min_training_rows <- max(1L, as.integer(config$min_training_rows))

  if (is.null(config$run_timestamp)) {
    config$run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  if (is.null(config$output_dir)) {
    config$output_dir <- file.path(config$output_root, paste0("run_", config$run_timestamp))
  }

  config
}

prepare_output_dir <- function(config) {
  dir.create(config$output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(config$output_dir, winslash = "/", mustWork = TRUE)
}

scalar_true <- function(value) {
  value <- as.logical(value)
  length(value) > 0L && !is.na(value[1L]) && isTRUE(value[1L])
}

load_market_data <- function(config) {
  cat("Loading market history from", config$data_start_date, "...\n")

  signal_data_raw <- tryCatch(
    {
      suppressWarnings(quantmod::getSymbols(
        config$signal_symbol,
        src = "yahoo",
        from = config$data_start_date,
        auto.assign = FALSE
      ))
    },
    error = function(error) {
      stop(
        "Could not download signal symbol ", config$signal_symbol,
        " from Yahoo. Check internet access and symbol availability.\n",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  filter_data_raw <- tryCatch(
    {
      suppressWarnings(quantmod::getSymbols(
        config$filter_symbol,
        src = "yahoo",
        from = config$data_start_date,
        auto.assign = FALSE
      ))
    },
    error = function(error) {
      stop(
        "Could not download filter symbol ", config$filter_symbol,
        " from Yahoo. Check internet access and symbol availability.\n",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  list(
    signal = stats::na.omit(signal_data_raw),
    filter = stats::na.omit(filter_data_raw)
  )
}

apply_kalman_filter <- function(time_series) {
  model <- dlm::dlmModPoly(order = 1, dV = 0.8, dW = 0.1)
  smoothed <- dlm::dlmSmooth(dlm::dlmFilter(time_series, model))
  xts::xts(smoothed$s[-1], order.by = zoo::index(time_series))
}

prepare_backtest_data <- function(s_data, f_data, params, b_start_date) {
  s_data$sma <- TTR::SMA(quantmod::Cl(s_data), n = as.integer(params[["sma_p"]]))

  raw_rsi <- TTR::RSI(quantmod::Cl(s_data), n = as.integer(params[["rsi_p"]]))
  s_data$rsi <- if (scalar_true(params[["use_kalman"]])) {
    apply_kalman_filter(raw_rsi)
  } else {
    raw_rsi
  }

  f_data$fast_ma <- TTR::SMA(quantmod::Cl(f_data), n = as.integer(params[["f_fast"]]))
  f_data$slow_ma <- TTR::SMA(quantmod::Cl(f_data), n = as.integer(params[["f_slow"]]))

  all_data <- merge(s_data, f_data[, c("fast_ma", "slow_ma")], join = "inner")
  all_data <- stats::na.omit(all_data)
  all_data[paste0(b_start_date, "/")]
}

run_strategy_backtest <- function(prepared_data, params, config) {
  if (nrow(prepared_data) < (config$holding_period + 1L)) {
    return(config$initial_capital)
  }

  current_capital <- config$initial_capital
  position_open <- FALSE
  entry_bar <- 0L

  for (i in 2:nrow(prepared_data)) {
    if (position_open && (i >= entry_bar + config$holding_period)) {
      position_open <- FALSE
    }

    if (!position_open) {
      filter_is_bullish <- scalar_true(prepared_data$fast_ma[i] > prepared_data$slow_ma[i])
      filter_is_bearish <- scalar_true(prepared_data$fast_ma[i] < prepared_data$slow_ma[i])

      buy_signal <- scalar_true(quantmod::Cl(prepared_data)[i] < prepared_data$sma[i]) &&
        scalar_true(prepared_data$rsi[i - 1L] < params[["rsi_os"]]) &&
        scalar_true(prepared_data$rsi[i] >= params[["rsi_os"]])

      sell_signal <- scalar_true(quantmod::Cl(prepared_data)[i] > prepared_data$sma[i]) &&
        scalar_true(prepared_data$rsi[i - 1L] > params[["rsi_ob"]]) &&
        scalar_true(prepared_data$rsi[i] <= params[["rsi_ob"]])

      direction <- "None"
      if (buy_signal && filter_is_bullish) direction <- "Long"
      if (sell_signal && filter_is_bearish) direction <- "Short"

      if (direction != "None") {
        if (i + config$holding_period > nrow(prepared_data)) next

        entry_price <- as.numeric(quantmod::Cl(prepared_data)[i])
        exit_price <- as.numeric(quantmod::Cl(prepared_data)[i + config$holding_period])
        if (is.na(entry_price) || is.na(exit_price) || entry_price == 0) next

        return_pct <- (exit_price / entry_price) - 1
        pnl <- current_capital * 0.20 * ifelse(direction == "Long", return_pct, -return_pct)
        current_capital <- current_capital + pnl
        position_open <- TRUE
        entry_bar <- i
      }
    }
  }

  current_capital
}

empty_phase1_trades <- function() {
  data.frame(
    EntryDate = as.Date(character()),
    Direction = character(),
    PnL = numeric()
  )
}

run_detailed_backtest_phase1 <- function(prepared_data, params, config) {
  equity_curve <- xts::xts(
    rep(config$initial_capital, nrow(prepared_data)),
    order.by = zoo::index(prepared_data)
  )
  trades <- list()

  if (nrow(prepared_data) < (config$holding_period + 1L)) {
    return(list(trades = empty_phase1_trades(), equity_curve = equity_curve))
  }

  current_capital <- config$initial_capital
  position_open <- FALSE
  entry_bar <- 0L

  for (i in 2:nrow(prepared_data)) {
    equity_curve[i] <- current_capital

    if (position_open && (i >= entry_bar + config$holding_period)) {
      position_open <- FALSE
    }

    if (!position_open) {
      filter_is_bullish <- scalar_true(prepared_data$fast_ma[i] > prepared_data$slow_ma[i])
      filter_is_bearish <- scalar_true(prepared_data$fast_ma[i] < prepared_data$slow_ma[i])

      buy_signal <- scalar_true(quantmod::Cl(prepared_data)[i] < prepared_data$sma[i]) &&
        scalar_true(prepared_data$rsi[i - 1L] < params[["rsi_os"]]) &&
        scalar_true(prepared_data$rsi[i] >= params[["rsi_os"]])

      sell_signal <- scalar_true(quantmod::Cl(prepared_data)[i] > prepared_data$sma[i]) &&
        scalar_true(prepared_data$rsi[i - 1L] > params[["rsi_ob"]]) &&
        scalar_true(prepared_data$rsi[i] <= params[["rsi_ob"]])

      direction <- "None"
      if (buy_signal && filter_is_bullish) direction <- "Long"
      if (sell_signal && filter_is_bearish) direction <- "Short"

      if (direction != "None") {
        if (i + config$holding_period > nrow(prepared_data)) next

        entry_price <- as.numeric(quantmod::Cl(prepared_data)[i])
        exit_price <- as.numeric(quantmod::Cl(prepared_data)[i + config$holding_period])
        if (is.na(entry_price) || is.na(exit_price) || entry_price == 0) next

        return_pct <- (exit_price / entry_price) - 1
        pnl <- current_capital * 0.20 * ifelse(direction == "Long", return_pct, -return_pct)
        current_capital <- current_capital + pnl
        position_open <- TRUE
        entry_bar <- i
        trades[[length(trades) + 1L]] <- data.frame(
          EntryDate = zoo::index(prepared_data)[i],
          Direction = direction,
          PnL = pnl
        )
      }
    }
  }

  if (nrow(equity_curve) > 0L) {
    equity_curve[nrow(equity_curve)] <- current_capital
  }

  trade_data <- if (length(trades) > 0L) do.call(rbind, trades) else empty_phase1_trades()
  list(trades = trade_data, equity_curve = equity_curve)
}

build_param_grid <- function(config) {
  param_grid <- expand.grid(
    sma_p = config$sma_period_range,
    rsi_p = config$rsi_period_range,
    rsi_os = config$rsi_oversold_range,
    rsi_ob = config$rsi_overbought_range,
    f_fast = config$filter_fast_range,
    f_slow = config$filter_slow_range,
    use_kalman = config$use_kalman_options,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  subset(param_grid, f_fast < f_slow)
}

parameter_ids <- function(params) {
  id_columns <- c("sma_p", "rsi_p", "rsi_os", "rsi_ob", "f_fast", "f_slow", "use_kalman")
  apply(params[, id_columns, drop = FALSE], 1L, paste, collapse = "-")
}

remaining_param_grid <- function(param_grid, results_so_far) {
  if (nrow(results_so_far) == 0L) {
    return(param_grid)
  }

  param_grid[!parameter_ids(param_grid) %in% parameter_ids(results_so_far), , drop = FALSE]
}

evaluate_parameter_set <- function(i, params_grid, signal_data_raw, filter_data_raw, config) {
  params <- params_grid[i, , drop = FALSE]
  prepared_data <- prepare_backtest_data(
    signal_data_raw,
    filter_data_raw,
    params,
    config$backtest_start_date
  )
  final_equity <- run_strategy_backtest(prepared_data, params, config)
  cbind(params, FinalEquity = final_equity)
}

run_optimization <- function(signal_data_raw, filter_data_raw, config, output_dir) {
  cat("Phase 1: starting parameter optimization...\n")

  checkpoint_file <- file.path(output_dir, "optimization_checkpoint.rds")
  results_so_far <- if (config$resume_checkpoint && file.exists(checkpoint_file)) {
    readRDS(checkpoint_file)
  } else {
    data.frame()
  }

  if (nrow(results_so_far) > 0L) {
    cat(nrow(results_so_far), "parameter combinations loaded from checkpoint.\n")
  }

  param_grid <- build_param_grid(config)
  if (nrow(param_grid) == 0L) {
    stop("The parameter grid is empty. Check fast/slow filter ranges.", call. = FALSE)
  }

  param_grid_to_run <- remaining_param_grid(param_grid, results_so_far)
  cat(nrow(param_grid), "total combinations,", nrow(param_grid_to_run), "remaining.\n")

  if (nrow(param_grid_to_run) == 0L) {
    optimization_results <- results_so_far
  } else {
    num_chunks <- ceiling(nrow(param_grid_to_run) / config$chunk_size)
    pb <- progress::progress_bar$new(
      format = "  optimization [:bar] :percent ETA: :eta",
      total = num_chunks,
      width = 80
    )

    cores <- min(config$num_cores, nrow(param_grid_to_run))
    cluster <- NULL

    if (cores > 1L) {
      cluster <- parallel::makeCluster(cores)
      on.exit({
        parallel::stopCluster(cluster)
        foreach::registerDoSEQ()
      }, add = TRUE)
      doParallel::registerDoParallel(cluster)
    }

    for (k in seq_len(num_chunks)) {
      start_row <- ((k - 1L) * config$chunk_size) + 1L
      end_row <- min(k * config$chunk_size, nrow(param_grid_to_run))
      current_chunk_params <- param_grid_to_run[start_row:end_row, , drop = FALSE]

      cat(
        "\nProcessing chunk", k, "of", num_chunks,
        "(", nrow(current_chunk_params), "combinations)...\n"
      )

      if (cores > 1L) {
        new_results <- foreach::foreach(
          i = seq_len(nrow(current_chunk_params)),
          .combine = "rbind",
          .packages = c("quantmod", "TTR", "dlm", "xts", "zoo"),
          .export = c(
            "evaluate_parameter_set", "prepare_backtest_data",
            "run_strategy_backtest", "apply_kalman_filter", "scalar_true"
          )
        ) %dopar% {
          evaluate_parameter_set(i, current_chunk_params, signal_data_raw, filter_data_raw, config)
        }
      } else {
        new_results <- do.call(
          rbind,
          lapply(
            seq_len(nrow(current_chunk_params)),
            evaluate_parameter_set,
            params_grid = current_chunk_params,
            signal_data_raw = signal_data_raw,
            filter_data_raw = filter_data_raw,
            config = config
          )
        )
      }

      results_so_far <- rbind(results_so_far, new_results)
      saveRDS(results_so_far, checkpoint_file)
      cat("Checkpoint saved with", nrow(results_so_far), "results.\n")
      pb$tick()
    }

    optimization_results <- results_so_far
  }

  best_params <- optimization_results[which.max(optimization_results$FinalEquity), , drop = FALSE]
  saveRDS(optimization_results, file.path(output_dir, "optimization_results.rds"))
  saveRDS(best_params, file.path(output_dir, "best_params.rds"))

  cat("\nPhase 1 finished. Best parameters saved.\n")
  list(results = optimization_results, best_params = best_params, checkpoint_file = checkpoint_file)
}

safe_numeric_metric <- function(expression, default = NA_real_) {
  tryCatch(as.numeric(force(expression))[1L], error = function(error) default)
}

create_phase1_report <- function(signal_data_raw, filter_data_raw, best_params, config, output_dir) {
  cat("\nCreating phase 1 report...\n")

  prepared_data_best <- prepare_backtest_data(
    signal_data_raw,
    filter_data_raw,
    best_params,
    config$backtest_start_date
  )
  detailed_results <- run_detailed_backtest_phase1(prepared_data_best, best_params, config)
  equity_curve_best <- detailed_results$equity_curve
  returns_best <- stats::na.omit(PerformanceAnalytics::Return.calculate(equity_curve_best))

  final_equity <- if (nrow(equity_curve_best) > 0L) {
    as.numeric(xts::last(equity_curve_best))
  } else {
    config$initial_capital
  }

  total_return <- final_equity / config$initial_capital - 1
  max_dd <- safe_numeric_metric(PerformanceAnalytics::maxDrawdown(returns_best))
  sharpe_ratio <- safe_numeric_metric(
    PerformanceAnalytics::SharpeRatio.annualized(returns_best, Rf = 0)
  )

  chart <- if (nrow(equity_curve_best) > 0L) {
    highcharter::hchart(equity_curve_best, name = "Equity") |>
      highcharter::hc_title(text = "Equity Curve") |>
      highcharter::hc_add_theme(highcharter::hc_theme_flat())
  } else {
    htmltools::tags$p("No equity curve was generated.")
  }

  report_html <- htmltools::tagList(
    htmltools::tags$h2("Phase 1 Report: Best Strategy"),
    htmltools::tags$hr(),
    htmltools::tags$p(paste("Kalman filter:", best_params$use_kalman)),
    htmltools::tags$p(paste(
      "SMA:", best_params$sma_p,
      "| RSI:", best_params$rsi_p,
      "| OS/OB:", paste0(best_params$rsi_os, "/", best_params$rsi_ob)
    )),
    htmltools::tags$hr(),
    htmltools::tags$h3("Performance Metrics"),
    htmltools::tags$table(
      class = "table",
      htmltools::tags$tr(
        htmltools::tags$td("Final equity:"),
        htmltools::tags$td(scales::dollar(final_equity))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Total return:"),
        htmltools::tags$td(scales::percent(total_return))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Maximum drawdown:"),
        htmltools::tags$td(scales::percent(max_dd))
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Annualized Sharpe ratio:"),
        htmltools::tags$td(round(sharpe_ratio, 2))
      )
    ),
    htmltools::tags$hr(),
    chart
  )

  html_file_path <- file.path(output_dir, "phase1_report.html")
  htmltools::save_html(report_html, file = html_file_path)
  saveRDS(detailed_results, file.path(output_dir, "phase1_detailed_backtest.rds"))
  cat("Phase 1 report saved:", normalizePath(html_file_path, winslash = "/"), "\n")

  list(
    detailed_results = detailed_results,
    report_file = html_file_path
  )
}

compute_garch_forecast <- function(signal_data_raw, config, output_dir) {
  cat("\nPhase 1.5: computing rolling GARCH forecast...\n")

  returns_garch <- stats::na.omit(quantmod::Delt(quantmod::Cl(signal_data_raw)))
  names(returns_garch) <- "Return"
  n <- nrow(returns_garch)
  forecasts <- rep(NA_real_, n)

  if (n <= config$garch_lookback) {
    warning("Not enough observations for GARCH lookback; forecast will be empty.")
    garch_vol_forecast <- xts::xts(forecasts, order.by = zoo::index(returns_garch))
    names(garch_vol_forecast) <- "GARCH.Forecast"
    saveRDS(garch_vol_forecast, file.path(output_dir, "garch_vol_forecast.rds"))
    return(garch_vol_forecast)
  }

  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    distribution.model = "norm"
  )

  forecast_indices <- seq.int(config$garch_lookback + 1L, n, by = config$garch_step)
  if (tail(forecast_indices, 1L) != n) {
    forecast_indices <- c(forecast_indices, n)
  }

  previous_forecast <- NA_real_

  for (position in seq_along(forecast_indices)) {
    i <- forecast_indices[position]
    window_returns <- returns_garch[(i - config$garch_lookback):(i - 1L), ]

    fit <- try(
      rugarch::ugarchfit(spec, data = window_returns, solver = "hybrid"),
      silent = TRUE
    )

    if (inherits(fit, "try-error")) {
      forecasts[i] <- if (is.na(previous_forecast)) {
        stats::sd(as.numeric(window_returns), na.rm = TRUE)
      } else {
        previous_forecast
      }
    } else {
      fore <- rugarch::ugarchforecast(fit, n.ahead = 1)
      forecasts[i] <- as.numeric(rugarch::sigma(fore)[1, 1])
    }

    previous_forecast <- forecasts[i]

    if (position %% config$garch_progress_every == 0L) {
      cat("GARCH progress:", round(position / length(forecast_indices) * 100), "%\r")
    }
  }

  garch_vol_forecast <- xts::xts(forecasts, order.by = zoo::index(returns_garch))
  garch_vol_forecast <- zoo::na.locf(garch_vol_forecast, na.rm = FALSE)
  names(garch_vol_forecast) <- "GARCH.Forecast"

  saveRDS(garch_vol_forecast, file.path(output_dir, "garch_vol_forecast.rds"))
  cat("\nGARCH forecast saved.\n")
  garch_vol_forecast
}

empty_ml_data <- function() {
  data.frame(
    GARCH_Forecast = numeric(),
    SMA_Dist = numeric(),
    RSI_Strength = numeric(),
    Vol_Trend = numeric(),
    OptimalLot = numeric()
  )
}

prepare_ml_data <- function(signal_data_raw, filter_data_raw, garch_vol_forecast, best_params, config, output_dir) {
  cat("Phase 2: preparing machine-learning data...\n")

  s_data <- signal_data_raw
  f_data <- filter_data_raw

  s_data$sma <- TTR::SMA(quantmod::Cl(s_data), n = as.integer(best_params$sma_p))
  raw_rsi <- TTR::RSI(quantmod::Cl(s_data), n = as.integer(best_params$rsi_p))
  s_data$rsi <- if (scalar_true(best_params$use_kalman)) {
    apply_kalman_filter(raw_rsi)
  } else {
    raw_rsi
  }

  f_data$fast_ma <- TTR::SMA(quantmod::Cl(f_data), n = as.integer(best_params$f_fast))
  f_data$slow_ma <- TTR::SMA(quantmod::Cl(f_data), n = as.integer(best_params$f_slow))

  all_data <- merge(s_data, f_data[, c("fast_ma", "slow_ma")], join = "inner")
  all_data <- merge(all_data, garch_vol_forecast, join = "inner")
  all_data$GARCH.Trend <- TTR::SMA(all_data$GARCH.Forecast, n = config$garch_trend_period)
  all_data <- stats::na.omit(all_data)
  all_data <- all_data[paste0(config$backtest_start_date, "/")]

  ml_data_list <- list()

  if (nrow(all_data) >= 2L) {
    avg_garch_vol <- mean(all_data$GARCH.Forecast, na.rm = TRUE)

    for (i in 2:nrow(all_data)) {
      filter_bullish <- scalar_true(all_data$fast_ma[i] > all_data$slow_ma[i])
      filter_bearish <- scalar_true(all_data$fast_ma[i] < all_data$slow_ma[i])

      buy_signal <- scalar_true(quantmod::Cl(all_data)[i] < all_data$sma[i]) &&
        scalar_true(all_data$rsi[i - 1L] < best_params$rsi_os) &&
        scalar_true(all_data$rsi[i] >= best_params$rsi_os)

      sell_signal <- scalar_true(quantmod::Cl(all_data)[i] > all_data$sma[i]) &&
        scalar_true(all_data$rsi[i - 1L] > best_params$rsi_ob) &&
        scalar_true(all_data$rsi[i] <= best_params$rsi_ob)

      if ((buy_signal && filter_bullish) || (sell_signal && filter_bearish)) {
        garch_feature <- as.numeric(all_data$GARCH.Forecast[i])
        if (!is.finite(garch_feature) || garch_feature <= 0) next

        sma_dist_feature <- abs(as.numeric(quantmod::Cl(all_data)[i]) / as.numeric(all_data$sma[i]) - 1)
        vol_trend_feature <- garch_feature - as.numeric(all_data$GARCH.Trend[i])

        rsi_strength_feature <- if (buy_signal) {
          best_params$rsi_os - as.numeric(all_data$rsi[i])
        } else {
          as.numeric(all_data$rsi[i]) - best_params$rsi_ob
        }

        optimal_lot <- config$min_lot_pct * (avg_garch_vol / garch_feature)
        optimal_lot <- max(config$min_lot_pct, min(config$max_lot_pct, optimal_lot))

        ml_data_list[[length(ml_data_list) + 1L]] <- data.frame(
          GARCH_Forecast = garch_feature,
          SMA_Dist = sma_dist_feature,
          RSI_Strength = rsi_strength_feature,
          Vol_Trend = vol_trend_feature,
          OptimalLot = optimal_lot
        )
      }
    }
  }

  ml_data <- if (length(ml_data_list) > 0L) do.call(rbind, ml_data_list) else empty_ml_data()

  saveRDS(all_data, file.path(output_dir, "ml_all_data.rds"))
  saveRDS(ml_data, file.path(output_dir, "ml_training_data.rds"))
  utils::write.csv(ml_data, file.path(output_dir, "ml_training_data.csv"), row.names = FALSE)

  cat(nrow(ml_data), "training examples generated.\n")
  list(all_data = all_data, ml_data = ml_data)
}

train_lot_size_model <- function(ml_data, config, output_dir) {
  cat("Training XGBoost lot-size model...\n")

  if (nrow(ml_data) <= config$min_training_rows) {
    cat("Not enough data for ML model. Falling back to minimum lot size.\n")
    saveRDS(NULL, file.path(output_dir, "lot_size_model.rds"))
    return(NULL)
  }

  train_size <- floor(0.75 * nrow(ml_data))
  train_set <- ml_data[1:train_size, ]
  train_features <- as.matrix(train_set[, c("GARCH_Forecast", "SMA_Dist", "RSI_Strength", "Vol_Trend")])
  train_labels <- train_set$OptimalLot

  lot_size_model <- xgboost::xgboost(
    data = train_features,
    label = train_labels,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )

  saveRDS(lot_size_model, file.path(output_dir, "lot_size_model.rds"))
  xgboost::xgb.save(lot_size_model, file.path(output_dir, "lot_size_model.xgb"))
  cat("Model training finished.\n")
  lot_size_model
}

empty_final_trades <- function() {
  data.frame(
    Date = as.Date(character()),
    PnL = numeric(),
    LotPct_as_Margin = numeric(),
    Equity = numeric()
  )
}

run_final_backtest <- function(all_data, best_params, lot_size_model, config, output_dir) {
  cat("Starting final backtest...\n")

  trades <- list()
  current_capital <- config$initial_capital

  if (nrow(all_data) <= config$holding_period + 1L) {
    trade_data <- empty_final_trades()
    saveRDS(trade_data, file.path(output_dir, "final_trades.rds"))
    utils::write.csv(trade_data, file.path(output_dir, "final_trades.csv"), row.names = FALSE)
    return(trade_data)
  }

  for (i in 2:(nrow(all_data) - config$holding_period)) {
    if (current_capital <= 0) break

    filter_bullish <- scalar_true(all_data$fast_ma[i] > all_data$slow_ma[i])
    filter_bearish <- scalar_true(all_data$fast_ma[i] < all_data$slow_ma[i])

    buy_signal <- scalar_true(quantmod::Cl(all_data)[i] < all_data$sma[i]) &&
      scalar_true(all_data$rsi[i - 1L] < best_params$rsi_os) &&
      scalar_true(all_data$rsi[i] >= best_params$rsi_os)

    sell_signal <- scalar_true(quantmod::Cl(all_data)[i] > all_data$sma[i]) &&
      scalar_true(all_data$rsi[i - 1L] > best_params$rsi_ob) &&
      scalar_true(all_data$rsi[i] <= best_params$rsi_ob)

    direction <- "None"
    if (buy_signal && filter_bullish) direction <- "Long"
    if (sell_signal && filter_bearish) direction <- "Short"

    if (direction != "None") {
      dynamic_lot_pct <- config$min_lot_pct

      if (!is.null(lot_size_model)) {
        sma_dist_live <- abs(as.numeric(quantmod::Cl(all_data)[i]) / as.numeric(all_data$sma[i]) - 1)
        vol_trend_live <- as.numeric(all_data$GARCH.Forecast[i]) - as.numeric(all_data$GARCH.Trend[i])

        rsi_strength_live <- if (direction == "Long") {
          best_params$rsi_os - as.numeric(all_data$rsi[i])
        } else {
          as.numeric(all_data$rsi[i]) - best_params$rsi_ob
        }

        features_for_prediction <- as.matrix(data.frame(
          GARCH_Forecast = as.numeric(all_data$GARCH.Forecast[i]),
          SMA_Dist = sma_dist_live,
          RSI_Strength = rsi_strength_live,
          Vol_Trend = vol_trend_live
        ))

        predicted_lot <- as.numeric(stats::predict(lot_size_model, features_for_prediction))[1L]
        if (is.finite(predicted_lot)) {
          dynamic_lot_pct <- max(config$min_lot_pct, min(config$max_lot_pct, predicted_lot))
        }
      }

      entry_price <- as.numeric(quantmod::Cl(all_data)[i])
      exit_price <- as.numeric(quantmod::Cl(all_data)[i + config$holding_period])
      if (is.na(entry_price) || is.na(exit_price) || entry_price == 0) next

      return_pct <- (exit_price / entry_price) - 1
      margin_used <- current_capital * dynamic_lot_pct
      position_value <- margin_used * config$leverage
      pnl <- position_value * ifelse(direction == "Long", return_pct, -return_pct)
      current_capital <- current_capital + pnl

      trades[[length(trades) + 1L]] <- data.frame(
        Date = zoo::index(all_data)[i],
        PnL = pnl,
        LotPct_as_Margin = dynamic_lot_pct * 100,
        Equity = current_capital
      )
    }
  }

  trade_data <- if (length(trades) > 0L) do.call(rbind, trades) else empty_final_trades()
  saveRDS(trade_data, file.path(output_dir, "final_trades.rds"))
  utils::write.csv(trade_data, file.path(output_dir, "final_trades.csv"), row.names = FALSE)
  trade_data
}

summarize_final_results <- function(best_params, trades, config, output_dir) {
  lines <- c("--- Final analysis ---")

  if (nrow(trades) > 0L) {
    final_equity <- tail(trades$Equity, 1L)
    total_return_pct <- (final_equity / config$initial_capital - 1) * 100

    lines <- c(
      lines,
      "Best base parameters:",
      capture.output(print(best_params)),
      paste("Initial capital:", scales::dollar(config$initial_capital)),
      paste("Final equity:", scales::dollar(final_equity)),
      paste("Total return:", paste0(round(total_return_pct, 2), "%")),
      "",
      "ML-selected margin statistics (%):",
      capture.output(print(summary(trades$LotPct_as_Margin)))
    )
  } else {
    lines <- c(
      lines,
      "No trades were executed in the final backtest.",
      paste("Initial capital:", scales::dollar(config$initial_capital)),
      paste("Final equity:", scales::dollar(config$initial_capital))
    )
  }

  lines <- c(lines, paste("Output directory:", normalizePath(output_dir, winslash = "/")))
  writeLines(lines, file.path(output_dir, "summary.txt"))
  lines
}

run_strategy <- function(config = list()) {
  load_strategy_packages()
  config <- normalize_strategy_config(config)
  output_dir <- prepare_output_dir(config)

  cat("Writing run outputs to", output_dir, "\n")
  saveRDS(config, file.path(output_dir, "run_config.rds"))

  market_data <- load_market_data(config)

  optimization <- run_optimization(
    market_data$signal,
    market_data$filter,
    config,
    output_dir
  )

  create_phase1_report(
    market_data$signal,
    market_data$filter,
    optimization$best_params,
    config,
    output_dir
  )

  garch_vol_forecast <- compute_garch_forecast(market_data$signal, config, output_dir)

  ml_inputs <- prepare_ml_data(
    market_data$signal,
    market_data$filter,
    garch_vol_forecast,
    optimization$best_params,
    config,
    output_dir
  )

  lot_size_model <- train_lot_size_model(ml_inputs$ml_data, config, output_dir)

  trades <- run_final_backtest(
    ml_inputs$all_data,
    optimization$best_params,
    lot_size_model,
    config,
    output_dir
  )

  summary_lines <- summarize_final_results(
    optimization$best_params,
    trades,
    config,
    output_dir
  )

  cat(paste(summary_lines, collapse = "\n"), "\n")
  cat("\n--- All operations finished ---\n")

  invisible(list(
    config = config,
    output_dir = output_dir,
    optimization = optimization,
    garch_vol_forecast = garch_vol_forecast,
    ml_data = ml_inputs$ml_data,
    trades = trades
  ))
}
