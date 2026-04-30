rsi_only_config <- list(
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
  output_root = "runs"
)
