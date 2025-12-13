module Web.View.Data.CandleChartHtml where

import Generated.Types
import IHP.ViewPrelude hiding (Symbol)
import Web.Routes
import Web.Types

candleChartHtml :: Maybe Text -> Html
candleChartHtml Nothing = mempty
candleChartHtml (Just code) = [hsx|
    <div class="mt-4">
      <h3>Chart: {code}</h3>

      <div id="tv-chart" data-code={code} style="height: 420px; width: 100%;"></div>

      <script src="/static/candle-chart.js"></script>
    </div>
  |]