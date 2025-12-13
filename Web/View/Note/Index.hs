module Web.View.Note.Index where
import IHP.ViewPrelude

data IndexView = IndexView

instance View IndexView where
  html IndexView = [hsx|
    <div class="row">
      <div class="ol-3 border-end">
        <h5>笔记列表</h5>
        <p>（左侧 Tree 将来放策略结构）</p>
      </div>
      <div class="col-9">
        <h3>笔记详情</h3>
        <p>类似 Jupyter Notebook 的策略内容展示。</p>
      </div>
    </div>
  |]