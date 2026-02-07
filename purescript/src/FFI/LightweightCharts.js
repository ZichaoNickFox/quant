// Minimal FFI stubs to bridge to the global LightweightCharts library.
// You can extend as needed.

const fallbackSeries = {
  setData: () => {},
};

const fallbackChart = {
  addCandlestickSeries: () => fallbackSeries,
  timeScale: () => ({ fitContent: () => {} }),
};

export const _createChart = (el) => () => {
  try {
    if (typeof LightweightCharts === "undefined" || !LightweightCharts.createChart) {
      console.warn("LightweightCharts is not available on window");
      return fallbackChart;
    }
    return LightweightCharts.createChart(el);
  } catch (e) {
    console.error("createChart error", e);
    return fallbackChart;
  }
};

export const _addCandlestickSeries = (chart) => () =>
  (() => {
    try {
      return chart.addCandlestickSeries();
    } catch (e) {
      console.error("addCandlestickSeries error", e);
      return fallbackSeries;
    }
  })();

export const _setDataFromJson = (series) => (json) => () => {
  try {
    const data = JSON.parse(json);
    series.setData(data);
  } catch (e) {
    console.error("setDataFromJson parse error", e);
  }
};

export const _fitContent = (chart) => () => {
  try {
    chart.timeScale().fitContent();
  } catch (e) {
    console.error("fitContent error", e);
  }
};
