import tushare as ts
import pandas as pd
from datetime import datetime
import json
import sys
import os

# yyyymmdd_to_iso
def yyyymmdd_to_iso(string):
    """
    Convert 'YYYYMMDD' -> 'YYYY-MM-DDT00:00:00'
    Return None for None / NaN / empty
    """
    if string is None or (isinstance(string, float) and pd.isna(string)) or string == "":
      return None
    else:
      return datetime.strptime(str(string), "%Y%m%d").strftime("%Y-%m-%dT%H:%M:%S")

# all stock https://www.tushare.pro/document/2?doc_id=25
def stock_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.stock_basic(exchange='', list_status='L', fields=['ts_code', 'name', 'list_date', 'delist_date']))
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "stock", "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(row[3])}
             for row in df.itertuples(index=False)]
  return symbols

# all index https://www.tushare.pro/document/2?doc_id=94
def index_basic():
  # 导入tushare
  import tushare as ts
  # 初始化pro接口
  pro = ts.pro_api()
  df = pd.DataFrame(pro.index_basic(fields=['ts_code', 'name', 'list_date']))
  df = df[df['list_date'].notna()]
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "index", "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(None)}
             for row in df.itertuples(index=False)]
  return symbols

# All etf https://tushare.pro/document/2?doc_id=385
def etf_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.etf_basic(exchange='', list_status='L', fields=['ts_code', 'extname', 'list_date']))
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "etf",  "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(None)}
             for row in df.itertuples(index=False)]
  return symbols

# All futures https://tushare.pro/document/2?doc_id=133
def future_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.fut_basic(exchange='', fut_type='', fields=['ts_code', 'name', 'list_date', 'delist_date']))
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "future", "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(row[3])}
             for row in df.itertuples(index=False)]
  return symbols

# All options https://tushare.pro/document/2?doc_id=163
def option_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.opt_basic(exchange='', fields=['ts_code', 'name', 'list_date', 'delist_date']))
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "option", "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(row[3])}
             for row in df.itertuples(index=False)]
  return symbols

# All funds https://tushare.pro/document/2?doc_id=19
def fund_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.fund_basic(market='', fields=['ts_code', 'name', 'list_date', 'delist_date']))
  df = df[df['list_date'].notna()]
  df = df[df['list_date'].astype(str).str.match(r'^\d{8}$')]
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "fund", "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(row[3])}
             for row in df.itertuples(index=False)]
  return symbols

# 宽基指数列表
def get_broad_based_index_list():
  data = [
    {"symbol_code": "000001.SH.INDEX", "symbol_name": "上证指数"},
    {"symbol_code": "399001.SZ.INDEX", "symbol_name": "深圳成指"},
    {"symbol_code": "399006.SZ.INDEX", "symbol_name": "创业板指"},
    {"symbol_code": "000300.SH.INDEX", "symbol_name": "沪深300"},
    {"symbol_code": "000905.SH.INDEX", "symbol_name": "中证500"},
  ]
  df = pd.DataFrame(data, columns=["symbol_code", "symbol_name"])
  return df

def get_symbol(symbol_type):
  if symbol_type == "stock":              # stock
    return stock_basic()
  elif symbol_type == "index":            # index
    return index_basic()
  elif symbol_type == "etf":              # etf
    return etf_basic()
  elif symbol_type == "future":           # future
    return future_basic()
  elif symbol_type == "option":           # option
    return option_basic()
  elif symbol_type == "fund":             # fund
    return fund_basic()
  elif symbol_type == "ShangHai":         # 上证股票
    return get_shanghai_stock_list()
  elif symbol_type == "ShenZhen":         # 深证股票
    return get_shenzhen_stock_list()
  elif symbol_type == "BroadBasedIndex":  # 宽基指数
    return get_broad_based_index_list()
  elif symbol_type == "IndustryIndex":    # 行业指数
    return get_industry_index_list()
  elif symbol_type == "ConceptIndex":     # 板块指数
    return get_concept_index_list()
  else:
    print("[get_symbol.py]", "error symbol_type : " + repr(symbol_type), file=sys.stderr)
    return []

if __name__ == "__main__":
  token = os.environ.get("TUSHARE_TOKEN")
  if not token:
    raise RuntimeError("TUSHARE_TOKEN not set")
  ts.set_token(token)
  symbol_type = ""
  if len(sys.argv) > 1:
    symbol_type = json.loads(sys.argv[1])
  else:
    assert "[fetch_symbols.py] parameter error"
  symbols = get_symbol(symbol_type)
  json_result = json.dumps(symbols, ensure_ascii = False)
  print(json_result)
