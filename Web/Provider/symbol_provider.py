import akshare as ak
import tushare as ts
import pandas as pd
from datetime import datetime
import json
import sys
import os

def dump(category, content):
  print(category, content, file=sys.stderr)

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

# All etf https://tushare.pro/document/2?doc_id=385
def etf_basic():
  pro = ts.pro_api()
  df = pd.DataFrame(pro.etf_basic(exchange='', list_status='L', fields=['ts_code', 'extname', 'list_date']))
  symbols = [{"code" : row[0], "name" : row[1], "symbol_type" : "etf",  "list_date" : yyyymmdd_to_iso(row[2]), "delist_date" : yyyymmdd_to_iso(None)}
             for row in df.itertuples(index=False)]
  return symbols

# 上证股票列表 https://akshare.akfamily.xyz/data/stock/stock.html#id229
def get_shanghai_stock_list():
  df = stock_info_sh_name_code_df = ak.stock_info_sh_name_code(symbol="主板A股")
  #       证券代码  证券简称                公司全称        上市日期
  # 0     600000  浦发银行      上海浦东发展银行股份有限公司  1999-11-10
  # 1     600004  白云机场      广州白云国际机场股份有限公司  2003-04-28
  # 2     600006  东风股份          东风汽车股份有限公司  1999-07-27
  # 3     600007  中国国贸      中国国际贸易中心股份有限公司  1999-03-12
  # 4     600008  首创环保    北京首创生态环保集团股份有限公司  2000-04-27
  # ...      ...   ...                 ...         ...
  # 1687  605580  恒盛能源          恒盛能源股份有限公司  2021-08-19
  # 1688  605588  冠石科技        南京冠石科技股份有限公司  2021-08-12
  # 1689  605589  圣泉集团        济南圣泉集团股份有限公司  2021-08-10
  # 1690  605598  上海港湾  上海港湾基础建设(集团)股份有限公司  2021-09-17
  # 1691  605599  菜百股份       北京菜市口百货股份有限公司  2021-09-09
  df = refactor_df_column(df, "证券代码", "证券简称", "XSHG")
  # symbol_code  symbol_name
  # 600000.XSHG  浦发银行
  # 600004.XSHG  白云机场
  # 600006.XSHG  东风股份
  # 600007.XSHG  中国国贸
  # 600008.XSHG  首创环保
  # ...   ..     ...
  # 605580.XSHG  恒盛能源
  # 605588.XSHG  冠石科技
  # 605589.XSHG  圣泉集团
  # 605598.XSHG  上海港湾
  # 605599.XSHG  菜百股份
  return df

# 深证股票列表 https://akshare.akfamily.xyz/data/stock/stock.html#id230
def get_shenzhen_stock_list():
  df = ak.stock_info_sz_name_code(symbol="A股列表")
  #        板块    A股代码   A股简称      A股上市日期           A股总股本          A股流通股本    所属行业
  # 0      主板  000001   平安银行  1991-04-03  19,405,918,198  19,405,571,850   J 金融业
  # 1      主板  000002  万  科Ａ  1991-01-29   9,724,196,533   9,716,935,865   K 房地产
  # 2      主板  000004   国华网安  1990-12-01     132,380,282     126,288,093  I 信息技术
  # 3      主板  000006   深振业Ａ  1992-04-27   1,349,995,046   1,349,987,396   K 房地产
  # 4      主板  000007    全新好  1992-04-13     346,448,044     346,448,044   K 房地产
  # ...   ...     ...    ...         ...             ...             ...     ...
  # 2841  创业板  301622    英思特  2024-12-04     115,931,880      27,488,084   C 制造业
  # 2842  创业板  301626   苏州天脉  2024-10-24     115,680,000      21,992,305   C 制造业
  # 2843  创业板  301628   强达电路  2024-10-31      75,375,800      18,844,000   C 制造业
  # 2844  创业板  301631   壹连科技  2024-11-22      65,296,129      13,028,302   C 制造业
  # 2845  创业板  301633   港迪技术  2024-11-07      55,680,000      13,920,000  I 信息技术
  df = df[df['A股代码'].str.startswith('0')]
  df = refactor_df_column(df, "A股代码", "A股简称", "XSHE")
  # symbol_code  symbol_name
  # 000001.XSHE  平安银行
  # 000002.XSHE  万  科Ａ
  # 000004.XSHE  国华网安
  # 000006.XSHE  深振业Ａ
  # 000007.XSHE  全新好
  # ...          ...
  # 301622.XSHE  英思特
  # 301626.XSHE  苏州天脉
  # 301628.XSHE  强达电路
  # 301631.XSHE  壹连科技
  # 301633.XSHE  港迪技术
  return df

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

# 行业列表 https://akshare.akfamily.xyz/data/stock/stock.html#id345
def get_industry_index_list():
  df = ak.stock_board_industry_name_em()
  #     排名   板块名称    板块代码       最新价      涨跌额   涨跌幅             总市值   换手率  上涨家数  下跌家数   领涨股票  领涨股票-涨跌幅
  # 0    1    贵金属  BK0732   1046.62    19.07  1.86    318311600000  2.96     9     3   晓程科技      6.90
  # 1    2   煤炭行业  BK0437  10498.13    -1.46 -0.01   1858556640000  1.27    18    15    宝泰隆      9.89
  # 2    3   化学制药  BK0465  26715.03  -186.57 -0.69   1578806112000  2.25    45    97   鲁抗医药      9.99
  # 3    4     银行  BK0475   3639.85   -33.55 -0.91  13153499904000  0.32     4    36   瑞丰银行      0.74
  # 4    5   工程机械  BK0739   1530.95   -15.12 -0.98    610291552000  1.76    12    20   长盛轴承     12.35
  # ..  ..    ...     ...       ...      ...   ...             ...   ...   ...   ...    ...       ...
  # 81  82   通信服务  BK0736    994.24   -56.12 -5.34   3484894096000  1.94     2    34  *ST鹏博      2.43
  # 82  83   旅游酒店  BK0485  13975.94  -825.17 -5.58    335325728000  3.88     0    37   锦江Ｂ股     -0.33
  # 83  84     教育  BK0740    594.36   -38.30 -6.05     77788967000  6.45     1    14   学大教育      1.75
  # 84  85  互联网服务  BK0447  20843.52 -1349.06 -6.08   1550683136000  5.49     6   142   金财互联      3.81
  # 85  86   商业百货  BK0482  17267.08 -1126.91 -6.13    570674080000  6.59     6    55   益民集团      9.96
  df["ipoDate"] = "1990-12-19"
  df = refactor_df_column(df, "板块代码", "板块名称", "")
  # symbol_code symbol_name
  # BK0732      贵金属
  # BK0437      煤炭行业
  # BK0465      化学制药
  # BK0475      银行
  # BK0739      工程机械
  # ..  ..      ...
  # BK0736      通信服务
  # BK0485      旅游酒店
  # BK0740      教育
  # BK0447      互联网服务
  # BK0482      商业百货
  return df

# 板块列表 https://akshare.akfamily.xyz/data/stock/stock.html#id345
def get_concept_index_list():
  df = ak.stock_board_concept_name_em()
  #       排名        板块名称    板块代码       最新价     涨跌额   涨跌幅           总市值    换手率  上涨家数  下跌家数   领涨股票  领涨股票-涨跌幅
  # 0      1    昨日连板_含一字  BK1051   1619.17   14.35  0.89  121946799000  17.50    13     7   梦洁股份      9.91
  # 1      2        超级真菌  BK0862    983.28    5.92  0.61  473355536000   3.11     7    13   鲁抗医药      9.99
  # 2      3        独家药品  BK0676   2271.80   -1.27 -0.06  719697680000   1.61    12    29   新华制药      9.98
  # 3      4    昨日涨停_含一字  BK1050  88853.88 -136.46 -0.15  359014624000  15.04    28    39   山子高科     10.00
  # 4      5         减肥药  BK1146    901.86   -3.47 -0.38  362659744000   2.21    10    16   普利制药     13.36
  # ..   ...         ...     ...       ...     ...   ...           ...    ...   ...   ...    ...       ...
  # 453  454  抖音概念(字节概念)  BK0923   1178.53  -80.64 -6.40  668380048000   7.09     3    73   捷成股份      6.32
  # 454  455       国资云概念  BK1008   1185.69  -82.45 -6.50  300268832000   3.20     0    17  *ST卓朗      0.00
  # 455  456        快手概念  BK0972   1344.29  -94.54 -6.57  231223755000   7.79     1    31   紫天科技      3.95
  # 456  457        盲盒经济  BK0954   1053.60  -84.46 -7.42  102849520000   6.03     0    11   芒果超媒     -3.10
  # 457  458        退税商店  BK0933   1230.17 -100.99 -7.59  145922129000   2.63     0    10   豫园股份     -3.15
  df["ipoDate"] = "1990-12-19"
  df = refactor_df_column(df, "板块代码", "板块名称", "")
  # symbol_code symbol_name
  # BK1051      昨日连板_含一字
  # BK0862      超级真菌
  # BK0676      独家药品
  # BK1050      昨日涨停_含一字
  # BK1146      减肥药
  # ..          ...
  # BK0923      抖音概念(字节概念)
  # BK1008      国资云概念
  # BK0972      快手概念
  # BK0954      盲盒经济
  # BK0933      退税商店
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
    dump("get_symbol.py", "error symbol_type : " + repr(symbol_type))

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