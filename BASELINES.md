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

### `baseline_wf_v1_20260502_091141`

- **Datum:** 2026-05-02 16:31 (Pod), 50.000 Param-Sets evaluiert (40k coarse + 10k refine), anschließend 5-Fold expanding-window Walk-Forward
- **Code-State:** Commit `b784e95` (Walk-Forward in `R/strategy_walk_forward.R` separiert)
- **Config:** [config/runpod.R](config/runpod.R) zum Commit-Stand `b784e95` (`walk_forward=TRUE`, `wf_n_folds=5L`)
- **Kerndaten EURUSD=X / GC=F**, 1995-01-01 bis Run-Datum, Backtest ab 1999-01-01
- **Beste Parameter (identisch zu `baseline_50k_v1`):**
  - `sma_p=60`, `rsi_p=10`, `rsi_os=30`, `rsi_ob=72`, `f_fast=46`, `f_slow=155`, `use_kalman=FALSE`
  - Phase-1 FinalEquity (In-Sample): $1.053,50
- **Final-Backtest (In-Sample, gesamtes Datenfenster):**
  - Initial $1.000 → Final **$4.416,13** (+341,61 %)
  - Profit Factor 1,874; 84 Trades (63 Long, 21 Short)
  - 10-Bar Hit-Rate 57,14 % (Long 53,97 %, Short 66,67 %)
- **Walk-Forward OOS (5 Folds, expanding window, Embargo = holding_period):**
  - Initial $1.000 → Chained Final **$1.401,00** (**+40,10 %** über alle Folds verkettet)
  - Mean per-fold Return: +8,84 % (sd 23,02)
  - Mean Hit-Rate 54,7 %; Mean Profit Factor 1,73
  - Per-Fold:
    | Fold | Train→Test | Trades | Return | PF | End Equity |
    |---|---|---|---|---|---|
    | 1 | →2013-10-30 / 2013-11–2016-02 | 10 | −5,60 % | 0,81 | $943,97 |
    | 2 | →2016-02-16 / 2016-03–2020-02 | 11 | +21,72 % | 1,60 | $1.149,01 |
    | 3 | →2020-02-24 / 2020-03–2022-05 | 11 | −14,86 % | 0,61 | $978,30 |
    | 4 | →2022-05-04 / 2022-05–2023-11 | 10 | +42,26 % | 4,61 | $1.391,72 |
    | 5 | →2023-11-30 / 2023-12–2026-03 | 11 | +0,67 % | 1,04 | $1.401,00 |
- **Beobachtung:** Großes IS↔OOS-Gap (+342 % vs +40 %) ⇒ deutliches Overfitting der Phase-1-Parameter. WF-Equity dennoch durchweg über Startkapital. 2 von 5 Folds negativ (Fold 1 & 3). Fold 4 dominanter Beitrag.
- **Files (zusätzlich zu Standard-Phase-1/ML/Final):**
  - `walk_forward/walk_forward_summary.txt`
  - `walk_forward/fold_metrics.{csv,rds}`, `walk_forward/combined_trades.{csv,rds}`, `walk_forward/oos_summary.rds`
  - `walk_forward/fold_NN/{train_set,full_path_trades,fold_trades}.{rds,csv}` (NN = 01..05)

