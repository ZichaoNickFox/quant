// Minimal FFI stubs to bridge to the global LightweightCharts library.
// You can extend as needed.

export const _createChart = (el) => () =>
  LightweightCharts.createChart(el);

export const _addCandlestickSeries = (chart) => () =>
  chart.addCandlestickSeries();

export const _setData = (series) => (arr) => () =>
  series.setData(arr);

export const _setDataFromJson = (series) => (json) => () => {
  try {
    const data = JSON.parse(json);
    series.setData(data);
  } catch (e) {
    console.error("setDataFromJson parse error", e);
  }
};

export const _fitContent = (chart) => () => chart.timeScale().fitContent();
