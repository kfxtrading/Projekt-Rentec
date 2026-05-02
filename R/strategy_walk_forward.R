# =============================================================================
# Walk-forward cross-validation extension for the RSI / GARCH / ML strategy.
#
# This file is a SELF-CONTAINED ADD-ON on top of R/strategy.R. The base
# strategy.R is intentionally kept free of walk-forward logic; everything
# specific to walk-forward CV lives here.
#
# Usage:
#   source("R/strategy.R")
#   source("R/strategy_walk_forward.R")
#   res <- run_strategy_walk_forward(strategy_config)
#
# Or set `walk_forward = TRUE` in the config and run via scripts/run_strategy.R
# (the runner sources this file when the flag is set).
#
# Approach:
#   * Phase 1 best params stay fixed (single-pass, optimised on full history).
#   * For each of K expanding-window folds we retrain ML/MM models on
#     ml_data[1..cut_idx[k]] and evaluate on the next slice.
#   * Embargo of `wf_embargo_bars` trading bars between train end and test
#     start to avoid leakage from overlapping holding-period returns.
#   * The OOS equity is *chained* across folds, starting from
#     config$initial_capital, mimicking sequential live deployment.
#
# Implementation note: instead of duplicating the whole final-backtest loop,
# we call the unmodified `run_final_backtest()` from strategy.R and then
# (1) restrict the trade frame to the test window and
# (2) rescale PnL/Equity to the chained OOS starting capital. This is exact
#     because lot fractions, return percentages and leverage are all
#     independent of the capital level at the time of trade.
# =============================================================================

# Default knobs for the walk-forward extension. Merged into a config in
# `run_strategy_walk_forward()` if the user has not set them explicitly.
default_walk_forward_config <- function() {
  list(
    walk_forward = TRUE,
    wf_n_folds = 5L,
    wf_embargo_bars = NULL,    # defaults to holding_period when NULL
    wf_min_train_rows = 30L
  )
}

# Per-fold OOS metrics from a (windowed, rescaled) trade frame.
compute_fold_metrics <- function(trades, fold_id, starting_capital) {
  if (nrow(trades) == 0L) {
    return(data.frame(
      Fold = fold_id, n_trades = 0L, return_pct = 0,
      hit_rate = NA_real_, profit_factor = NA_real_,
      avg_win = NA_real_, avg_loss = NA_real_,
      start_equity = starting_capital, end_equity = starting_capital,
      stringsAsFactors = FALSE
    ))
  }
  end_eq <- trades$Equity[nrow(trades)]
  wins <- trades$PnL[trades$PnL > 0]
  losses <- trades$PnL[trades$PnL < 0]
  data.frame(
    Fold = fold_id,
    n_trades = nrow(trades),
    return_pct = if (starting_capital > 0) (end_eq / starting_capital - 1) * 100 else NA_real_,
    hit_rate = sum(trades$PnL > 0) / nrow(trades),
    profit_factor = if (length(losses) > 0L) sum(wins) / abs(sum(losses)) else Inf,
    avg_win = if (length(wins) > 0L) mean(wins) else NA_real_,
    avg_loss = if (length(losses) > 0L) mean(losses) else NA_real_,
    start_equity = starting_capital,
    end_equity = end_eq,
    stringsAsFactors = FALSE
  )
}

# Restrict a trade frame produced by run_final_backtest() to a date window
# and rescale PnL / Equity to a different starting capital. Lot percentage,
# direction and entry/exit prices are preserved because they are not
# capital-dependent in the strategy.
.wf_window_and_rescale <- function(trades, win_start, win_end, leverage,
                                   starting_capital) {
  if (nrow(trades) == 0L) return(trades)
  trades$Date <- as.Date(trades$Date)
  trades <- trades[trades$Date >= win_start & trades$Date <= win_end, , drop = FALSE]
  if (nrow(trades) == 0L) return(trades)

  cap <- starting_capital
  for (j in seq_len(nrow(trades))) {
    if (cap <= 0) {
      trades <- trades[seq_len(j - 1L), , drop = FALSE]
      break
    }
    lot_pct <- trades$LotPct_as_Margin[j] / 100
    sign <- if (trades$Direction[j] == "Long") 1 else -1
    entry <- trades$EntryClose[j]
    exit  <- trades$ExitClose[j]
    if (!is.finite(entry) || !is.finite(exit) || entry == 0) {
      trades$PnL[j] <- 0
      trades$Equity[j] <- cap
      next
    }
    ret_pct <- (exit / entry - 1) * sign
    pnl <- cap * lot_pct * leverage * ret_pct
    cap <- cap + pnl
    trades$PnL[j] <- pnl
    trades$Equity[j] <- cap
  }
  trades
}

# Expanding-window walk-forward driver. `all_data` and `ml_data` are the
# outputs of strategy.R's pipeline (ml_inputs$all_data / ml_inputs$ml_data).
run_walk_forward <- function(all_data, ml_data, best_params, config, output_dir) {
  if (!isTRUE(config$walk_forward)) return(NULL)

  cat("\n=== Walk-forward cross-validation ===\n")

  n_folds      <- as.integer(if (is.null(config$wf_n_folds)) 5L else config$wf_n_folds)
  embargo_bars <- as.integer(if (is.null(config$wf_embargo_bars)) config$holding_period else config$wf_embargo_bars)
  min_train    <- as.integer(if (is.null(config$wf_min_train_rows)) max(30L, config$min_training_rows) else config$wf_min_train_rows)

  if (n_folds < 2L) {
    cat("walk_forward requires wf_n_folds >= 2; skipping.\n"); return(NULL)
  }
  if (nrow(ml_data) < min_train * 2L) {
    cat(sprintf("Not enough ml_data rows (%d) for walk-forward (need >= %d). Skipping.\n",
                nrow(ml_data), min_train * 2L))
    return(NULL)
  }
  if (!"Date" %in% colnames(ml_data)) {
    cat("ml_data has no Date column; cannot run walk-forward.\n"); return(NULL)
  }

  ml_dates <- as.Date(ml_data$Date)
  ord <- order(ml_dates)
  ml_data <- ml_data[ord, , drop = FALSE]
  ml_dates <- ml_dates[ord]

  cut_idx <- floor(seq(min_train, nrow(ml_data), length.out = n_folds + 1L))
  cut_idx <- unique(cut_idx)
  cut_idx <- cut_idx[cut_idx >= min_train & cut_idx <= nrow(ml_data)]
  if (length(cut_idx) < 2L) {
    cat("Not enough unique fold boundaries; skipping walk-forward.\n"); return(NULL)
  }

  wf_dir <- file.path(output_dir, "walk_forward")
  dir.create(wf_dir, showWarnings = FALSE, recursive = TRUE)

  all_dates <- as.Date(zoo::index(all_data))

  fold_results <- list()
  combined_trades <- list()
  running_capital <- config$initial_capital

  fold_config <- config
  fold_config$resume_checkpoint <- FALSE  # never reuse caches across folds

  n_segments <- length(cut_idx) - 1L
  for (k in seq_len(n_segments)) {
    train_end_idx  <- cut_idx[k]
    train_end_date <- ml_dates[train_end_idx]

    test_end_date <- if (k == n_segments) ml_dates[length(ml_dates)] else ml_dates[cut_idx[k + 1L]]

    last_train_bar  <- max(which(all_dates <= train_end_date))
    test_start_bar  <- min(last_train_bar + embargo_bars + 1L, length(all_dates))
    test_start_date <- all_dates[test_start_bar]

    if (!is.finite(test_start_date) || test_start_date >= test_end_date) {
      cat(sprintf("Fold %d: empty test window after embargo, skipping.\n", k)); next
    }

    train_set <- ml_data[1:train_end_idx, , drop = FALSE]
    cat(sprintf("\n--- Fold %d/%d ---  train rows: %d  train end: %s  test: %s .. %s\n",
                k, n_segments, nrow(train_set), format(train_end_date),
                format(test_start_date), format(test_end_date)))

    fold_dir <- file.path(wf_dir, sprintf("fold_%02d", k))
    dir.create(fold_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(train_set, file.path(fold_dir, "train_set.rds"))

    fold_lot_model  <- train_lot_size_model(train_set, fold_config, fold_dir)
    fold_mm_models  <- train_money_management_models(train_set, fold_config, fold_dir)

    # Run the unmodified base backtest, then trim + rescale to the OOS window.
    full_trades <- run_final_backtest(
      all_data, best_params, fold_lot_model, fold_config, fold_dir,
      mm_models = fold_mm_models
    )
    # Rename base output so the in-window file is the canonical fold result.
    base_rds <- file.path(fold_dir, "final_trades.rds")
    base_csv <- file.path(fold_dir, "final_trades.csv")
    if (file.exists(base_rds)) file.rename(base_rds, file.path(fold_dir, "full_path_trades.rds"))
    if (file.exists(base_csv)) file.rename(base_csv, file.path(fold_dir, "full_path_trades.csv"))

    fold_trades <- .wf_window_and_rescale(
      full_trades, test_start_date, test_end_date,
      leverage = config$leverage, starting_capital = running_capital
    )
    saveRDS(fold_trades, file.path(fold_dir, "fold_trades.rds"))
    utils::write.csv(fold_trades, file.path(fold_dir, "fold_trades.csv"), row.names = FALSE)

    fold_metric <- compute_fold_metrics(fold_trades, k, running_capital)
    if (nrow(fold_trades) > 0L) {
      running_capital <- tail(fold_trades$Equity, 1L)
      combined_trades[[length(combined_trades) + 1L]] <- cbind(Fold = k, fold_trades)
    }
    fold_results[[length(fold_results) + 1L]] <- fold_metric

    cat(sprintf("Fold %d done: %d trades, return %.2f%%, hit-rate %.1f%%, PF %.2f, equity %.2f\n",
                k, fold_metric$n_trades, fold_metric$return_pct,
                fold_metric$hit_rate * 100, fold_metric$profit_factor,
                fold_metric$end_equity))
  }

  if (length(fold_results) == 0L) {
    cat("No folds produced metrics; walk-forward aborted.\n"); return(NULL)
  }

  fold_table <- do.call(rbind, fold_results)
  combined   <- if (length(combined_trades) > 0L) do.call(rbind, combined_trades) else NULL

  saveRDS(fold_table, file.path(wf_dir, "fold_metrics.rds"))
  utils::write.csv(fold_table, file.path(wf_dir, "fold_metrics.csv"), row.names = FALSE)
  if (!is.null(combined)) {
    saveRDS(combined, file.path(wf_dir, "combined_trades.rds"))
    utils::write.csv(combined, file.path(wf_dir, "combined_trades.csv"), row.names = FALSE)
  }

  oos <- list(
    fold_table = fold_table,
    combined_trades = combined,
    n_folds_used = nrow(fold_table),
    mean_return_pct   = mean(fold_table$return_pct, na.rm = TRUE),
    sd_return_pct     = stats::sd(fold_table$return_pct, na.rm = TRUE),
    median_return_pct = stats::median(fold_table$return_pct, na.rm = TRUE),
    mean_hit_rate     = mean(fold_table$hit_rate, na.rm = TRUE),
    mean_profit_factor = mean(fold_table$profit_factor[is.finite(fold_table$profit_factor)],
                              na.rm = TRUE),
    initial_capital   = config$initial_capital,
    final_oos_equity  = running_capital,
    chained_return_pct = if (config$initial_capital > 0)
      (running_capital / config$initial_capital - 1) * 100 else NA_real_
  )
  saveRDS(oos, file.path(wf_dir, "oos_summary.rds"))

  lines <- c(
    "Walk-forward cross-validation summary",
    "-------------------------------------",
    sprintf("Folds completed: %d", oos$n_folds_used),
    sprintf("Initial capital: %s", scales::dollar(oos$initial_capital)),
    sprintf("Chained OOS equity: %s   Total OOS return: %.2f%%",
            scales::dollar(oos$final_oos_equity), oos$chained_return_pct),
    sprintf("Per-fold return: mean %.2f%%  median %.2f%%  sd %.2f%%",
            oos$mean_return_pct, oos$median_return_pct, oos$sd_return_pct),
    sprintf("Per-fold hit-rate: mean %.1f%%   Per-fold profit factor: mean %.2f",
            oos$mean_hit_rate * 100, oos$mean_profit_factor),
    "",
    "Per-fold table:"
  )
  fold_lines <- utils::capture.output(print(fold_table, row.names = FALSE))
  writeLines(c(lines, fold_lines), file.path(wf_dir, "walk_forward_summary.txt"))

  cat("\n=== Walk-forward summary ===\n")
  cat(sprintf("Folds: %d   Mean OOS return: %.2f%% (sd %.2f)   Chained: %.2f%%\n",
              oos$n_folds_used, oos$mean_return_pct, oos$sd_return_pct,
              oos$chained_return_pct))
  cat(sprintf("Mean hit-rate: %.1f%%   Mean profit factor: %.2f\n",
              oos$mean_hit_rate * 100, oos$mean_profit_factor))
  cat(sprintf("Combined OOS equity: %s -> %s\n",
              scales::dollar(oos$initial_capital), scales::dollar(oos$final_oos_equity)))

  oos
}

# Top-level wrapper: run the regular strategy, then the walk-forward CV on
# top using the outputs of run_strategy().
run_strategy_walk_forward <- function(config = list()) {
  defaults <- default_walk_forward_config()
  for (nm in names(defaults)) {
    if (is.null(config[[nm]])) config[[nm]] <- defaults[[nm]]
  }

  res <- run_strategy(config)

  # run_strategy() merges defaults internally; re-merge here so wf knobs
  # survive. The returned res$config carries the merged config.
  merged_cfg <- res$config
  for (nm in names(defaults)) {
    if (is.null(merged_cfg[[nm]])) merged_cfg[[nm]] <- defaults[[nm]]
  }

  wf <- run_walk_forward(
    res$all_data,
    res$ml_data,
    res$optimization$best_params,
    merged_cfg,
    res$output_dir
  )

  res$walk_forward <- wf
  invisible(res)
}
