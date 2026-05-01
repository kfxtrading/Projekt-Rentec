#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

CONFIG_PATH="${1:-config/default.R}"
TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
LOG_DIR="runs/logs"
LOG_FILE="${LOG_DIR}/full_strategy_${TIMESTAMP}.log"
PID_FILE="${LOG_DIR}/full_strategy_${TIMESTAMP}.pid"

mkdir -p "$LOG_DIR"

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"

nohup Rscript scripts/run_strategy.R "$CONFIG_PATH" > "$LOG_FILE" 2>&1 &
PID="$!"
echo "$PID" > "$PID_FILE"

echo "Started full strategy run."
echo "PID: $PID"
echo "Log: $LOG_FILE"
echo "Follow progress with: tail -f $LOG_FILE"
