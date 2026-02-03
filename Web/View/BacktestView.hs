module Web.View.BacktestView where
import IHP.ViewPrelude

data BacktestView = BacktestView

instance View BacktestView where
    html BacktestView = [hsx|
        <h3>回测中心</h3>
        <p>选择策略、设置参数、运行回测、展示收益图。</p>
    |]
