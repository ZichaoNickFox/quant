from datetime import datetime
import json
import sys
import tushare as ts
import os

def dump(content):
  print(content, file=sys.stdout)
  sys.stdout.flush()

timeframe_map = ["1min", "5min", "15min", "30min", "60min", "D", "W", "M"]

def ts_to_json_format(df, ts_symbol_code_suffix, timeframe):
  res = []
  for _, row in df.iterrows():
    dt = datetime.strptime(row["trade_date"], "%Y%m%d")
    res.append({
      "symbol_id": row["symbol_id"],
      "timeframe": timeframe,
      "datetime": datetime.strftime(dt, "%Y-%m-%d %H:%M:%S"),
      "open": row["open"],
      "close": row["close"],
      "high": row["high"],
      "low": row["low"],
      "volume": row["vol"],
      "amount": row["amount"],
    })
  return res

if __name__ == "__main__":
  token = os.environ.get("TUSHARE_TOKEN")
  if not token:
    raise RuntimeError("TUSHARE_TOKEN not set")
  ts.set_token(token)
  parameter_json = ""
  if len(sys.argv) > 1:
    parameter_json = sys.argv[1]
  parameter_json = json.loads(sys.argv[1])
  symbol_code = parameter_json["symbol_code"]
  symbol_id = parameter_json.get("symbol_id")
  is_index = "INDEX" in symbol_code
  ts_symbol_code = symbol_code.replace("XSHG", "SH")
  ts_symbol_code = ts_symbol_code.replace("XSHE", "SZ")
  ts_symbol_code = ts_symbol_code.replace(".INDEX", "")
  timeframe = parameter_json["timeframe"]
  ts_timeframe = timeframe_map[timeframe]
  start_datetime = parameter_json["from_datetime"]
  start_datetime = start_datetime.replace("T", " ")
  end_datetime = parameter_json["to_datetime"]
  end_datetime = end_datetime.replace("T", " ")
  start_date = start_datetime[:10].replace("-", "")
  end_date = end_datetime[:10].replace("-", "")
  
  if is_index and ts_timeframe == "D":
    pro = ts.pro_api()
    df = pro.index_daily(ts_code = ts_symbol_code,
                         start_date = start_date,
                         end_date = end_date)
    # attach symbol_id and timeframe to rows for downstream parsing
    df["symbol_id"] = symbol_id
    result = ts_to_json_format(df, ".INDEX", timeframe)
    json_result = json.dumps(result, ensure_ascii = False)
    print(json_result)
  else:
    assert False
