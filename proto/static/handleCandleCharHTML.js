function handleCandleChartHTMLs() {
  const elements = document.querySelectorAll('[data-candle-chart]');
  if (!elements.length) return;

  elements.forEach(el => {
    handleCandleChartHTML(el);
  });
}

function handleCandleChartHTML(el) {
  const symbolType = el.dataset.symbolType; // etf
  const symbolCode = el.dataset.symbolCode; // 159915.SZ

  const chart = LightweightCharts.createChart(el, {
    width: el.clientWidth,
    height: el.clientHeight,
    layout: { background: { color: "#253248" }, textColor: "#83A869" },
    grid: { vertLines: { color: "#334158" }, horzLines: { color: "#334158" } },
    crosshair: { mode: LightweightCharts.CrosshairMode.Magnet },
    rightPriceScale: { autoScale: true, borderColor: "#485c7b" },
    timeScale: { borderColor: "#485c7b" },
  });
  const series =chart.addSeries(LightweightCharts.CandlestickSeries, {});
  const data = [
    { time: 1618602300, open: 0.039278, high: 0.039303, low: 0.039251, close: 0.039262 },
    { time: 1618602360, open: 0.039261000000000004, high: 0.039287, low: 0.039247000000000004, close: 0.039264 },
    { time: 1618602420, open: 0.039263, high: 0.039274, low: 0.039257, close: 0.039257 },
    { time: 1618602480, open: 0.039257, high: 0.039307999999999996, low: 0.039257, close: 0.039267 },
    { time: 1618602540, open: 0.039264999999999994, high: 0.0393, low: 0.039264999999999994, close: 0.039299 }
  ];
  series.setData(data);
  // fetch(`/Candles?symbolType=${encodeURIComponent(symbolType)}&symbolCode=${encodeURIComponent(symbolCode)}`)
  //   .then(r => r.json())
  //   .then(data => { series.setData(data); })
  //   .catch(console.error);
  chart.timeScale().fitContent();
}