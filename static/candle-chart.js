// static/app/candle-chart.js
import { createChart } from 'lightweight-charts';

(function () {
  const el = document.getElementById('tv-chart');
  if (!el) return;

  const code = el.dataset.code;

  const chart = createChart(el, { autoSize: true });
  const series = chart.addCandlestickSeries();

  fetch(`/api/candles?code=${encodeURIComponent(code)}`)
    .then(r => r.json())
    .then(data => {
      // data 形如 [{ time: 1700000000, open, high, low, close }, ...]
      series.setData(data);
    })
    .catch(console.error);
})();