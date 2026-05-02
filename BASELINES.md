# Baseline Runs

Schreibgeschützte Referenz-Snapshots vergangener Trainings/Optimierungen. Diese Verzeichnisse dürfen **nie überschrieben** werden — sie dienen als Vergleichsbasis für künftige Parameter-Tunings, Walk-Forward-Tests und Algorithm-Änderungen.

Lokal liegen die Daten unter `runs/baselines/` (per `.gitignore` ausgenommen, daher **nicht im Repo**). Files sind unter Windows mit `IsReadOnly=$true` markiert.

## Wiederherstellung / neuer Snapshot

```powershell
# Neuen Baseline aus aktuellem Run einfrieren:
$src = "runs\run_YYYYMMDD_HHMMSS"
$dst = "runs\baselines\baseline_<TAG>_$((Get-Date).ToString('yyyyMMdd_HHmmss'))"
Copy-Item -Recurse $src $dst
Get-ChildItem -Recurse $dst -File | ForEach-Object { $_.IsReadOnly = $true }

# Nur Lesen (zum Editieren in Code aufheben falls nötig):
Get-ChildItem -Recurse $dst -File | ForEach-Object { $_.IsReadOnly = $false }
```

## Auf RunPod (Pod) sichern

Die Network-Volume `/workspace` überlebt Pod-Stops. Sicherungspfad: `/workspace/Rentec/runs/baselines/`.

```bash
ssh runpod-rentec '
  SRC=/workspace/Rentec/runs/run_20260501_230547
  DST=/workspace/Rentec/runs/baselines/baseline_50k_v1_20260501_230547
  mkdir -p /workspace/Rentec/runs/baselines
  cp -r "$SRC" "$DST"
  chmod -R a-w "$DST"
'
```

---

## Manifest

### `baseline_50k_v1_20260501_230547`

- **Datum:** 2026-05-02 06:24 (Pod), 50.000 Param-Sets evaluiert (40k coarse + 10k refine)
- **Code-State:** Commit `37704ef` (Kelly + hit-rate + coarse-to-fine + xgboost 2.x)
- **Config:** [config/runpod.R](config/runpod.R) zum Commit-Stand `37704ef`
- **Kerndaten EURUSD=X / GC=F**, 1995-01-01 bis Run-Datum, Backtest ab 1999-01-01
- **Beste Parameter:**
  - `sma_p=60`, `rsi_p=10`, `rsi_os=30`, `rsi_ob=72`, `f_fast=46`, `f_slow=155`, `use_kalman=FALSE`
  - Phase-1 FinalEquity: $1.053,50
- **Final-Backtest:**
  - Initial $1.000 → Final **$4.416,67** (+341,67 %)
  - Profit Factor 1,874
  - 84 Signale (63 Long, 21 Short)
  - 1-Tag Hit-Rate 42,86 %; 10-Bar Hit-Rate 57,14 % (Long 53,97 %, **Short 66,67 %**)
  - Avg Win $152,63; Avg Loss −$108,60
- **ML Money Management:**
  - Kelly-Fraction 0,25, `combine_lot_models=TRUE` → Final Lot ≈ `min(kelly, voltarget)`
  - Win-Prob Median 70,7 %, Vol-Target-Lot Median 2,43 %, Final Margin Median 2,01 %
- **Files:**
  - `summary.txt`, `phase1_report.html`, `final_trades.csv`, `final_trades.rds`
  - `optimization_results.rds`, `optimization_checkpoint.rds`, `phase1_detailed_backtest.rds`
  - `garch_vol_forecast.rds`, `ml_training_data.{csv,rds}`, `ml_all_data.rds`
  - `lot_size_model.{rds,xgb}`, `money_mgmt_models.rds`, `best_params.rds`, `run_config.rds`

