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

### `baseline_wf_v1_gbpusd_20260502_143643`

- **Datum:** 2026-05-02 18:36 (Pod), Per-Symbol-Training für GBPUSD
- **Code-State:** Commit `01dda8c` (gleiche WF-Architektur wie EURUSD-Baseline)
- **Config:** [config/runpod_gbpusd.R](config/runpod_gbpusd.R) (`signal_symbol="GBPUSD=X"`, Filter `GC=F`, sonst identisch zu `runpod.R`)
- **Beste Parameter (eigene Optimierung, 50k Sets):**
  - `sma_p=110`, `rsi_p=12`, `rsi_os=30`, `rsi_ob=70`, `f_fast=38`, `f_slow=65`, `use_kalman=FALSE`
  - Phase-1 FinalEquity (In-Sample): $1.068,55
- **Final-Backtest (In-Sample, gesamtes Datenfenster):**
  - Initial $1.000 → Final **$6.390,12** (+539,01 %)
  - 61 Trades (36 Long, 25 Short)
  - 10-Bar Hit-Rate 65,57 % (Long 61,11 %, Short 72,00 %)
- **Walk-Forward OOS (5 Folds, expanding window, Embargo = holding_period):**
  - Initial $1.000 → Chained Final **$1.654,38** (**+65,44 %**)
  - Mean per-fold Return: +11,55 % (sd 17,12)
  - Mean Hit-Rate 56,3 %; Mean Profit Factor 3,59
  - Per-Fold:
    | Fold | Train→Test | Trades | Return | PF | End Equity |
    |---|---|---|---|---|---|
    | 1 | →2015-06-22 / 2015-07–2018-04 | 6 | +5,07 % | 1,19 | $1.050,68 |
    | 2 | →2018-04-17 / 2018-05–2019-08 | 6 | −3,54 % | 0,81 | $1.013,43 |
    | 3 | →2019-08-15 / 2019-09–2021-12 | 6 | +11,77 % | 1,92 | $1.132,72 |
    | 4 | →2021-12-07 / 2021-12–2023-06 | 5 | +3,89 % | 1,73 | $1.176,77 |
    | 5 | →2023-06-20 / 2023-07–2025-11 | 7 | +40,59 % | 12,27 | $1.654,38 |
- **Beobachtung:** Robuster als EURUSD (4/5 positive Folds vs 3/5; Chained +65 % vs +40 %). IS-OOS-Gap (+539 % vs +65 %) bleibt aber gross → Phase-1-Overfitting weiterhin präsent. Fold 5 dominanter Beitrag (PF 12.27).
- **Files:** wie `baseline_wf_v1_20260502_091141`

### `baseline_wf_v1_usdjpy_20260502_184848`

- **Datum:** 2026-05-02 22:48 (Pod), Per-Symbol-Training für USDJPY
- **Code-State:** Commit `43cf74f`
- **Config:** [config/runpod_usdjpy.R](config/runpod_usdjpy.R) (`signal_symbol="USDJPY=X"`, Filter `GC=F`)
- **Beste Parameter (eigene Optimierung, 50k Sets):**
  - `sma_p=125`, `rsi_p=14`, `rsi_os=30`, `rsi_ob=70`, `f_fast=26`, `f_slow=60`, `use_kalman=TRUE` (erstes Pair mit Kalman!)
  - Phase-1 FinalEquity (In-Sample): $1.044,77
- **Final-Backtest (In-Sample):**
  - Initial $1.000 → Final **$2.968,58** (+196,86 %)
  - 23 Trades (9 Long, 14 Short) — **deutlich weniger Signale** als EURUSD (84) / GBPUSD (61)
  - 10-Bar Hit-Rate 65,22 % (Long 55,56 %, Short 71,43 %)
  - Profit Factor 3,970 (höchster aller Pairs in IS)
- **Walk-Forward: ⚠️ ÜBERSPRUNGEN** — `Not enough ml_data rows (23) for walk-forward (need >= 60)`
  - Strategy-Code-Schwelle: `wf_min_train_rows * (n_folds + 1) = 30 * 6 = 60` ML-Zeilen erforderlich, USDJPY hat nur 23
  - Konsequenz: keine OOS-Validierung möglich. Pair generiert zu wenig Signale für 5-Fold-CV.
- **Files:** wie `baseline_wf_v1_20260502_091141` aber **ohne `walk_forward/` Unterordner**

