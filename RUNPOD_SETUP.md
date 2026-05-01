# Runpod Setup

## Recommended Pod

- Type: Pod, not Serverless
- GPU: RTX 3090 24 GB
- Reason: this strategy is mostly CPU-bound, and the RTX 3090 Pod commonly provides more vCPU/RAM than RTX 4090 Pods
- Container disk: 100 GB
- Network volume: 50-100 GB mounted under `/workspace`
- Template: Ubuntu or PyTorch/CUDA template with SSH/web terminal

## First-Time Setup On The Pod

Open the Runpod web terminal or connect through SSH, then clone or upload this repository to `/workspace/Rentec`.

From the repository root:

```bash
chmod +x scripts/setup_runpod_ubuntu.sh scripts/run_full_strategy.sh
./scripts/setup_runpod_ubuntu.sh
```

The setup script installs Ubuntu system libraries, installs the R project packages, and runs a package import smoke test.

## Start The Full Optimization

```bash
./scripts/run_full_strategy.sh config/runpod.R
```

Watch the log:

```bash
tail -f runs/logs/full_strategy_*.log
```

Outputs are written under `runs/run_<timestamp>/`. The optimization checkpoint is saved as `optimization_checkpoint.rds`, so an interrupted run can continue if the same output folder/checkpoint is reused.
