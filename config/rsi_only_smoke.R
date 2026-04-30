rsi_only_config <- list(
  initial_capital = 1000,
  holding_period = 10,
  data_start_date = "2023-01-01",
  backtest_start_date = "2024-01-01",
  leverage = 200,
  lot_pct = 0.02,
  signal_symbol = "EURUSD=X",
  rsi_period_range = c(10, 14),
  rsi_oversold_range = c(24, 30),
  rsi_overbought_range = c(70, 80),
  output_root = "runs"
)
