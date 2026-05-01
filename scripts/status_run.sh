#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "== Rscript processes =="
ps aux | grep '[R]script' || true

echo
echo "== Latest full-strategy log =="
LATEST_LOG="$(ls -t runs/logs/full_strategy_*.log 2>/dev/null | head -1 || true)"
if [ -n "$LATEST_LOG" ]; then
  echo "$LATEST_LOG"
  tail -n 60 "$LATEST_LOG"
else
  echo "No full-strategy log found."
fi

echo
echo "== Latest checkpoint =="
LATEST_CHECKPOINT="$(find runs -name optimization_checkpoint.rds -type f -printf '%T@ %p\n' 2>/dev/null | sort -nr | awk 'NR==1 {print $2}' || true)"
if [ -z "$LATEST_CHECKPOINT" ]; then
  echo "No optimization checkpoint found yet."
  exit 0
fi

echo "$LATEST_CHECKPOINT"

Rscript --vanilla - "$LATEST_CHECKPOINT" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
checkpoint <- args[[1]]
total <- 5189184

info <- file.info(checkpoint)
rows <- tryCatch(nrow(readRDS(checkpoint)), error = function(error) NA_integer_)

if (is.na(rows)) {
  cat("Could not read checkpoint rows.\n")
  quit(status = 0)
}

percent <- rows / total * 100
cat("Completed combinations:", rows, "of", total, sprintf("(%.2f%%)", percent), "\n")
cat("Checkpoint modified:", format(info$mtime, "%Y-%m-%d %H:%M:%S %Z"), "\n")

run_dir <- dirname(checkpoint)
config_file <- file.path(run_dir, "run_config.rds")
if (file.exists(config_file)) {
  config <- tryCatch(readRDS(config_file), error = function(error) NULL)
  if (!is.null(config)) {
    cat("Configured cores:", config$num_cores, "\n")
    cat("Chunk size:", config$chunk_size, "\n")
    cat("Output dir:", normalizePath(run_dir, winslash = "/", mustWork = FALSE), "\n")
  }
}

if (rows > 0) {
  elapsed <- as.numeric(difftime(Sys.time(), info$ctime, units = "secs"))
  if (is.finite(elapsed) && elapsed > 0) {
    rate <- rows / elapsed
    remaining <- (total - rows) / rate
    cat("Approx. rate:", sprintf("%.1f combinations/sec", rate), "\n")
    cat("Approx. ETA:", format(Sys.time() + remaining, "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("Approx. remaining:", sprintf("%.1f hours", remaining / 3600), "\n")
  }
}
RSCRIPT
