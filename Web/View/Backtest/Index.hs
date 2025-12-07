module Web.View.Backtest.Index where
import Web.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = [hsx|
        <h3>回测中心</h3>
        <p>选择策略、设置参数、运行回测、展示收益图。</p>
    |]