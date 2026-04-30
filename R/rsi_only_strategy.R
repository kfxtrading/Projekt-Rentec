required_rsi_only_packages <- function() {
  c("quantmod", "TTR", "scales", "PerformanceAnalytics", "xts", "zoo")
}

load_rsi_only_packages <- function(packages = required_rsi_only_packages()) {
  installed_packages <- rownames(utils::installed.packages())
  missing_packages <- setdiff(packages, installed_packages)

  if (length(missing_packages) > 0L) {
    stop(
      "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
      "Run scripts/setup_packages.R before running the RSI-only strategy.",
      call. = FALSE
    )
  }

  suppressWarnings(suppressMessages(suppressPackageStartupMessages({
    for (package in packages) {
      library(package, character.only = TRUE)
    }
  })))
}

rsi_only_default_config <- function() {
  list(
    initial_capital = 1000,
    holding_period = 10,
    data_start_date = "1995-01-01",
    backtest_start_date = "1999-01-01",
    leverage = 200,
    lot_pct = 0.02,
    signal_symbol = "EURUSD=X",
    rsi_period_range = seq(10, 14, by = 2),
    rsi_oversold_range = seq(20, 30, by = 2),
    rsi_overbought_range = seq(70, 90, by = 2),
    output_root = "runs",
    run_timestamp = NULL,
    output_dir = NULL
  )
}

merge_rsi_only_config <- function(config) {
  defaults <- rsi_only_default_config()

  for (name in names(config)) {
    defaults[[name]] <- config[[name]]
  }

  defaults
}

normalize_rsi_only_config <- function(config = list()) {
  config <- merge_rsi_only_config(config)

  config$holding_period <- max(1L, as.integer(config$holding_period))
  config$lot_pct <- max(0, as.numeric(config$lot_pct))
  config$leverage <- max(0, as.numeric(config$leverage))

  if (is.null(config$run_timestamp)) {
    config$run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  if (is.null(config$output_dir)) {
    config$output_dir <- file.path(config$output_root, paste0("rsi_only_run_", config$run_timestamp))
  }

  config
}

prepare_rsi_only_output_dir <- function(config) {
  dir.create(config$output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(config$output_dir, winslash = "/", mustWork = TRUE)
}

scalar_true <- function(value) {
  value <- as.logical(value)
  length(value) > 0L && !is.na(value[1L]) && isTRUE(value[1L])
}

load_rsi_market_data <- function(config) {
  cat("Loading signal history for", config$signal_symbol, "from", config$data_start_date, "...\n")

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

  stats::na.omit(signal_data_raw)
}

prepare_rsi_data <- function(signal_data_raw, params, backtest_start_date) {
  signal_data <- signal_data_raw
  signal_data$rsi <- TTR::RSI(
    quantmod::Cl(signal_data),
    n = as.integer(params[["rsi_p"]])
  )

  signal_data <- stats::na.omit(signal_data)
  signal_data[paste0(backtest_start_date, "/")]
}

rsi_signal_direction <- function(prepared_data, i, params) {
  buy_signal <- scalar_true(prepared_data$rsi[i - 1L] < params[["rsi_os"]]) &&
    scalar_true(prepared_data$rsi[i] >= params[["rsi_os"]])

  sell_signal <- scalar_true(prepared_data$rsi[i - 1L] > params[["rsi_ob"]]) &&
    scalar_true(prepared_data$rsi[i] <= params[["rsi_ob"]])

  if (buy_signal) {
    return("Long")
  }

  if (sell_signal) {
    return("Short")
  }

  "None"
}

empty_rsi_trades <- function() {
  data.frame(
    EntryDate = as.Date(character()),
    ExitDate = as.Date(character()),
    Direction = character(),
    EntryPrice = numeric(),
    ExitPrice = numeric(),
    ReturnPct = numeric(),
    PnL = numeric(),
    LotPct_as_Margin = numeric(),
    Equity = numeric()
  )
}

run_rsi_backtest <- function(prepared_data, params, config) {
  equity_curve <- xts::xts(
    rep(config$initial_capital, nrow(prepared_data)),
    order.by = zoo::index(prepared_data)
  )
  trades <- list()

  if (nrow(prepared_data) <= config$holding_period + 1L) {
    return(list(
      final_equity = config$initial_capital,
      trades = empty_rsi_trades(),
      equity_curve = equity_curve
    ))
  }

  current_capital <- config$initial_capital
  position_open <- FALSE
  position <- NULL

  for (i in 2:nrow(prepared_data)) {
    if (position_open && i >= position$exit_bar) {
      exit_price <- as.numeric(quantmod::Cl(prepared_data)[i])

      if (!is.na(exit_price) && !is.na(position$entry_price) && position$entry_price != 0) {
        return_pct <- (exit_price / position$entry_price) - 1
        margin_used <- position$capital_at_entry * config$lot_pct
        position_value <- margin_used * config$leverage
        pnl <- position_value * ifelse(position$direction == "Long", return_pct, -return_pct)
        current_capital <- current_capital + pnl

        trades[[length(trades) + 1L]] <- data.frame(
          EntryDate = position$entry_date,
          ExitDate = zoo::index(prepared_data)[i],
          Direction = position$direction,
          EntryPrice = position$entry_price,
          ExitPrice = exit_price,
          ReturnPct = return_pct,
          PnL = pnl,
          LotPct_as_Margin = config$lot_pct * 100,
          Equity = current_capital
        )
      }

      position_open <- FALSE
      position <- NULL
    }

    equity_curve[i] <- current_capital

    if (!position_open && i <= nrow(prepared_data) - config$holding_period) {
      direction <- rsi_signal_direction(prepared_data, i, params)

      if (direction != "None") {
        entry_price <- as.numeric(quantmod::Cl(prepared_data)[i])

        if (!is.na(entry_price) && entry_price != 0) {
          position_open <- TRUE
          position <- list(
            entry_bar = i,
            exit_bar = i + config$holding_period,
            entry_date = zoo::index(prepared_data)[i],
            entry_price = entry_price,
            direction = direction,
            capital_at_entry = current_capital
          )
        }
      }
    }
  }

  trade_data <- if (length(trades) > 0L) do.call(rbind, trades) else empty_rsi_trades()

  list(
    final_equity = current_capital,
    trades = trade_data,
    equity_curve = equity_curve
  )
}

build_rsi_param_grid <- function(config) {
  param_grid <- expand.grid(
    rsi_p = config$rsi_period_range,
    rsi_os = config$rsi_oversold_range,
    rsi_ob = config$rsi_overbought_range,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  subset(param_grid, rsi_os < rsi_ob)
}

run_rsi_optimization <- function(signal_data_raw, config, output_dir) {
  cat("Starting RSI-only parameter optimization...\n")

  param_grid <- build_rsi_param_grid(config)
  if (nrow(param_grid) == 0L) {
    stop("The RSI parameter grid is empty. Check RSI threshold ranges.", call. = FALSE)
  }

  results <- vector("list", nrow(param_grid))

  for (i in seq_len(nrow(param_grid))) {
    params <- param_grid[i, , drop = FALSE]
    prepared_data <- prepare_rsi_data(signal_data_raw, params, config$backtest_start_date)
    backtest <- run_rsi_backtest(prepared_data, params, config)

    results[[i]] <- cbind(
      params,
      FinalEquity = backtest$final_equity,
      Trades = nrow(backtest$trades)
    )
  }

  optimization_results <- do.call(rbind, results)
  best_params <- optimization_results[which.max(optimization_results$FinalEquity), , drop = FALSE]

  saveRDS(optimization_results, file.path(output_dir, "rsi_only_optimization_results.rds"))
  utils::write.csv(
    optimization_results,
    file.path(output_dir, "rsi_only_optimization_results.csv"),
    row.names = FALSE
  )
  saveRDS(best_params, file.path(output_dir, "rsi_only_best_params.rds"))

  cat("RSI-only optimization finished.\n")
  list(results = optimization_results, best_params = best_params)
}

safe_numeric_metric <- function(expression, default = NA_real_) {
  tryCatch(as.numeric(force(expression))[1L], error = function(error) default)
}

summarize_rsi_only_results <- function(best_params, backtest, config, output_dir) {
  equity_curve <- backtest$equity_curve
  returns <- stats::na.omit(PerformanceAnalytics::Return.calculate(equity_curve))
  final_equity <- backtest$final_equity
  total_return_pct <- (final_equity / config$initial_capital - 1) * 100
  max_dd <- safe_numeric_metric(PerformanceAnalytics::maxDrawdown(returns))
  sharpe_ratio <- safe_numeric_metric(
    PerformanceAnalytics::SharpeRatio.annualized(returns, Rf = 0)
  )

  lines <- c(
    "--- RSI-only analysis ---",
    paste("Signal symbol:", config$signal_symbol),
    paste("Best RSI period:", best_params$rsi_p),
    paste("Best RSI oversold threshold:", best_params$rsi_os),
    paste("Best RSI overbought threshold:", best_params$rsi_ob),
    paste("Initial capital:", scales::dollar(config$initial_capital)),
    paste("Final equity:", scales::dollar(final_equity)),
    paste("Total return:", paste0(round(total_return_pct, 2), "%")),
    paste("Trades:", nrow(backtest$trades)),
    paste("Maximum drawdown:", scales::percent(max_dd)),
    paste("Annualized Sharpe ratio:", round(sharpe_ratio, 2)),
    paste("Fixed margin per trade:", paste0(config$lot_pct * 100, "%")),
    paste("Leverage:", config$leverage),
    paste("Output directory:", normalizePath(output_dir, winslash = "/"))
  )

  writeLines(lines, file.path(output_dir, "rsi_only_summary.txt"))
  lines
}

save_rsi_only_backtest_outputs <- function(backtest, output_dir) {
  saveRDS(backtest$trades, file.path(output_dir, "rsi_only_trades.rds"))
  utils::write.csv(backtest$trades, file.path(output_dir, "rsi_only_trades.csv"), row.names = FALSE)

  saveRDS(backtest$equity_curve, file.path(output_dir, "rsi_only_equity_curve.rds"))
  utils::write.csv(
    data.frame(
      Date = zoo::index(backtest$equity_curve),
      Equity = as.numeric(backtest$equity_curve)
    ),
    file.path(output_dir, "rsi_only_equity_curve.csv"),
    row.names = FALSE
  )
}

run_rsi_only_strategy <- function(config = list()) {
  load_rsi_only_packages()
  config <- normalize_rsi_only_config(config)
  output_dir <- prepare_rsi_only_output_dir(config)

  cat("Writing RSI-only run outputs to", output_dir, "\n")
  saveRDS(config, file.path(output_dir, "rsi_only_run_config.rds"))

  signal_data_raw <- load_rsi_market_data(config)
  optimization <- run_rsi_optimization(signal_data_raw, config, output_dir)

  best_prepared_data <- prepare_rsi_data(
    signal_data_raw,
    optimization$best_params,
    config$backtest_start_date
  )
  best_backtest <- run_rsi_backtest(best_prepared_data, optimization$best_params, config)

  save_rsi_only_backtest_outputs(best_backtest, output_dir)
  summary_lines <- summarize_rsi_only_results(
    optimization$best_params,
    best_backtest,
    config,
    output_dir
  )

  cat(paste(summary_lines, collapse = "\n"), "\n")
  cat("\n--- RSI-only run finished ---\n")

  invisible(list(
    config = config,
    output_dir = output_dir,
    optimization = optimization,
    backtest = best_backtest
  ))
}
