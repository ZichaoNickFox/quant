INSERT INTO candle (symbol_id, timeframe, datetime, open, close, high, low, volume, amount)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (symbol_id, timeframe, datetime) DO UPDATE SET
  open   = EXCLUDED.open,
  close  = EXCLUDED.close,
  high   = EXCLUDED.high,
  low    = EXCLUDED.low,
  volume = EXCLUDED.volume,
  amount = EXCLUDED.amount;
