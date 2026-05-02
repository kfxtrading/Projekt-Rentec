#!/usr/bin/env bash
# Waits for an in-progress setup_runpod_ubuntu.sh to finish, then launches the
# full strategy run. Logs everything to runs/logs/chain.log.
set -u
cd "$(dirname "$0")/.."
LOG=runs/logs
mkdir -p "$LOG"
echo "[chain] $(date -Is) waiting for setup_runpod_ubuntu.sh ..." >> "$LOG/chain.log"
while pgrep -f setup_runpod_ubuntu.sh > /dev/null; do
  sleep 15
done
echo "[chain] $(date -Is) setup process finished" >> "$LOG/chain.log"
if ! grep -q 'Runpod setup finished.' "$LOG/setup.log"; then
  echo "[chain] $(date -Is) setup did NOT finish cleanly; aborting." >> "$LOG/chain.log"
  exit 1
fi
echo "[chain] $(date -Is) starting run_full_strategy.sh config/runpod.R" >> "$LOG/chain.log"
./scripts/run_full_strategy.sh config/runpod.R >> "$LOG/chain.log" 2>&1
echo "[chain] $(date -Is) run_full_strategy exit=$?" >> "$LOG/chain.log"
