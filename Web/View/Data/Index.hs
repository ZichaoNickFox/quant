module Web.View.Data.Index where
import Web.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = [hsx|
        <h3>数据管理</h3>
        <p>下载数据、检查覆盖范围、修复缺失、展示 Candle 表格...</p>
    |]