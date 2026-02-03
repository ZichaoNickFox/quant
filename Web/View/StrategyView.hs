module Web.View.StrategyView where
import IHP.ViewPrelude

data StrategyView = StrategyView

instance View StrategyView where
  html StrategyView = [hsx|
    <div class="row">
      <div class="col-3 border-end">
        <h5>策略列表</h5>
        <p>（左侧 Tree 将来放策略结构）</p>
      </div>
      <div class="col-9">
        <h3>策略详情</h3>
        <p>类似 Jupyter Notebook 的策略内容展示。</p>
      </div>
    </div>
  |]
