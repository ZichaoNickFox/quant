import tushare as ts
import pandas as pd
from datetime import datetime
import json
import sys
import os

# 重新设置列名
def refactor_df_column(df, symbol_column_name, name_column_name, symbol_suffix):
  if symbol_suffix == "":
    return pd.DataFrame(
      { "symbol_code" : (df[symbol_column_name].astype(str))
      , "symbol_name" : df[name_column_name].astype(str)
      }
    )
  else:
    return pd.DataFrame(
      { "symbol_code" : (df[symbol_column_name].astype(str) + "." + symbol_suffix)
      , "symbol_name" : df[name_column_name].astype(str)
      }
    )

# yyyymmdd_to_iso
def yyyymmdd_to_iso(string):
    """
    Convert 'YYYYMMDD' -> 'YYYY-MM-DDT00:00:00'
    Return None for None / NaN / empty
    """
    if string is None:
      return None
    else:
      return datetime.strptime(string, "%Y%m%d").strftime("%Y-%m-%dT%H:%M:%S")

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


def test(symbol_type):
  if symbol_type == "ShangHai":
    return \
      [
        [ "600000.XSHG"
        , 0
        , "浦发银行"
        ],
        [ "600004.XSHG"
        , 0
        , "白云机场"
        ]
      ]
  elif symbol_type == "ShenZhen":
    return \
      [
        [ "000001.XSHE"
        , 0
        , "平安银行"
        ],
        [ "000002.XSHE"
        , 0
        , "万科A"
        ]
      ]
  elif symbol_type == "BroadBasedIndex":
    return \
      [
        [ "000001.XSHG"
        , 1
        , "上证指数"
        ],
        [ "399001.XSHE"
        , 1
        , "深圳成指"
        ]
      ]
  elif symbol_type == "IndestryIndex":
    return \
      [
        [ "BK0732"
        , 1
        , "贵金属"
        ],
        [ "BK0437"
        , 1
        , "煤炭行业"
        ]
      ]
  elif symbol_type == "ConceptIndex":
    return \
      [
        [ "BK1051"
        , 1
        , "昨日连板_含一字"
        ],
        [ "BK0862"
        , 1
        , "超级真菌"
        ]
      ]

def get_symbol(symbol_type):
  if symbol_type == "stock":              # stock
    return stock_basic()
  elif symbol_type == "index":            # index
    return index_basic()
  elif symbol_type == "etf":              # etf
    return etf_basic()
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

if __name__ == "__main__":
  token = os.environ.get("TUSHARE_TOKEN")
  if not token:
    raise RuntimeError("TUSHARE_TOKEN not set")
  ts.set_token(token)
  symbol_type = ""
  if len(sys.argv) > 1:
    symbol_type = json.loads(sys.argv[1])
  else:
    assert "[get_symbols.py] parameter error"
  symbols = get_symbol(symbol_type)
  json_result = json.dumps(symbols, ensure_ascii = False)
  print(json_result)
