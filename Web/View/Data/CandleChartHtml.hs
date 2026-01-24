module Web.View.Data.CandleChartHtml where

import Web.Prelude
import Web.Types

candleChartHtml :: SelectedSymbol -> Html
candleChartHtml (SelectedSymbol symbolType symbolCode) = [hsx|
    <div class="mt-4">
      <div data-candle-chart data-symbol-type={inputValue symbolType} data-symbol-code={symbolCode} style="height: 420px; width: 100%;"></div>
    </div>
  |]