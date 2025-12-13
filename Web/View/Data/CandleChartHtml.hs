module Web.View.Data.CandleChartHtml where

import Generated.Types
import IHP.ViewPrelude hiding (Symbol)
import Web.Routes
import Web.Types

candleChartHtml :: Maybe Text -> Html
candleChartHtml Nothing = mempty
candleChartHtml (Just symbolCode) = [hsx|
    <div class="mt-4">
      <h3>Chart: {symbolCode}</h3>
 
      <div id="tv-chart" data-code={symbolCode} style="height: 420px; width: 100%;"></div>

      <script type="module">
        import chartsModule from 'https://cdn.jsdelivr.net/npm/lightweight-charts@5.0.9/+esm'

        function createCharts() {
          const el = document.getElementById('tv-chart');
          if (!el) return;

          // 159915.SZ
          const code = el.dataset.code;

          const chart = chartsModule(el, { autoSize: true });
          const series = chart.addCandlestickSeries();

          fetch(`/api/candles?code=${encodeURIComponent(code)}`)
            .then(r => r.json())
            .then(data => {
              // data 形如 [{ time: 1700000000, open, high, low, close }, ...]
              series.setData(data);
            })
            .catch(console.error);
        };

        document.addEventListener('turbolinks:load', () => {
          createCharts();
        });
      </script>
    </div>
  |]