from datetime import datetime
from efinance.utils import search_quote
import json
import requests
import sys
import tushare as ts
import os
import akshare as ak
import pandas as pd

def dump(content):
  print(content, file=sys.stdout)
  sys.stdout.flush()

# Output
def ak_to_json_format(df):
  res = []
  for _, row in df.iterrows():
    res.append({
      "symbol_id": row["symbol_id"],
      "timeframe": row["timeframe"],
      "datetime": row["datetime"],
      "open": row["开盘"],
      "close": row["收盘"],
      "high": row["最高"],
      "low": row["最低"],
      "volume": row["成交量"],
      "amount": row["成交额"],
    })
  return res

# 历史行情数据-东财
# stock_zh_a_hist
# https://akshare.akfamily.xyz/data/stock/stock.html#id22
def get_a_stock_candle_daily_weekly_monthly(symbol_id, symbol_code, timeframe, start_date, end_date, adjust):
  df = ak.stock_zh_a_hist(
    symbol = symbol_code,
    period = timeframe_map[timeframe],
    start_date = start_date,
    end_date = end_date,
    adjust = adjust)
  df["symbol_id"] = symbol_id
  df["timeframe"] = timeframe
  df.rename(columns={"日期": "datetime"}, inplace=True)
  return ak_to_json_format(df)

# 请问如何获取历史某一天的1分钟数据-efinance
# get_quote_history_1_minutes
# https://github.com/Micro-sheep/efinance/issues/35
def get_a_stock_candle_minutly(symbol, date):
  q = search_quote(symbol)
  df = pd.DataFrame(columns=['股票名称', '股票代码', '日期', '时间', '最新价', '均价', '成交量', '昨收', '开盘'])
  if not q:
      return df
  data = {
      'Day': date,
      'PhoneOSNew': '2',
      'StockID': q.code,
      'Token': '0',
      'UserID': '0',
      'VerSion': '5.2.1.0',
      'a': 'GetStockTrend',
      'apiv': 'w28',
      'c': 'StockL2History'
  }
  url = 'https://apphis.longhuvip.com/w1/api/index.php'
  response = requests.post(url, data = data)

  try:
    js = response.json()
  except:
    return df
  if not js.get('trend'):
    return df
  trend = pd.DataFrame(js['trend'])
  df[['时间', '最新价', '均价', '成交量']] = trend.values[:, :4]
  df['日期'] = datetime.strptime(js['day'], '%Y%m%d').strftime('%Y-%m-%d')
  df['昨收'] = js['preclose_px']
  df['开盘'] = js['begin_px']
  df['股票代码'] = q.code
  df['股票名称'] = q.name
  return df

# 历史行情数据-通用
# index_zh_a_hist
# https://akshare.akfamily.xyz/data/index/index.html#id7
def get_a_index_candle_daily_weely_monthly(symbol_id, symbol_code, timeframe, start_date, end_date):
  df = ak.index_zh_a_hist(
    symbol = symbol_code,
    period = timeframe_map[timeframe],
    start_date = start_date,
    end_date = end_date)
  df["symbol_id"] = symbol_id
  df["timeframe"] = timeframe
  df.rename(columns={"日期": "datetime"}, inplace=True)
  return ak_to_json_format(df)

def test_data():
  df = pd.DataFrame({
    "symbol_id": ['00000000-0000-0000-0000-000000000000'] * 5,
    "timeframe" : [6, 6, 6, 6, 6],
    "datetime" : ['2017-03-01T00:00:00', '2017-03-02T00:00:00', '2017-03-03T00:00:00', '2017-03-06T00:00:00', '2017-03-07T00:00:00'],
    "开盘" : [1575.20, 1578.45, 1562.20, 1560.57, 1567.07],
    "收盘" : [1575.20, 1565.45, 1560.57, 1568.70, 1568.70],
    "最高" : [1584.95, 1583.32, 1565.45, 1570.32, 1570.32],
    "最低" : [1571.95, 1563.82, 1554.07, 1558.95, 1560.57],
    "成交量" : [346994, 403629, 342655, 404511, 294673],
    "成交额" : [330157968.0, 382395888.0, 321952544.0, 381212304.0, 277747408.0],
  })
  return ak_to_json_format(df)

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
  
  if is_index and ts_timeframe == "D":
    pro = ts.pro_api()
    df = pro.index_daily(ts_code = ts_symbol_code,
                         start_date = start_datetime,
                         end_date = end_datetime)
    # attach symbol_id and timeframe to rows for downstream parsing
    df["symbol_id"] = symbol_id
    result = ts_to_json_format(df, ".INDEX", timeframe)
    json_result = json.dumps(result, ensure_ascii = False)
    print(json_result)
  else:
    assert False
