// Ensure we only bind once per element
function initFRP() {
  if (typeof handleCandleChartHTMLs === 'function') {
    handleCandleChartHTMLs();
  }
  if (typeof window.dataInit === 'function') {
    window.dataInit();
  }
  attachNotifyWS();
}

document.addEventListener('DOMContentLoaded', initFRP);
document.addEventListener('ihp:afterRender', initFRP);

// ---- WebSocket for symbols updates ----
let notifyWS = null;
function attachNotifyWS() {
  if (notifyWS) return; // single connection
  const loc = window.location;
  const proto = loc.protocol === 'https:' ? 'wss' : 'ws';
  const url = `${proto}://${loc.host}/ws/notify`;
  try {
    notifyWS = new WebSocket(url);
    notifyWS.onmessage = (ev) => {
      try {
        const msg = JSON.parse(ev.data);
        const type = msg.type || '';
        // Dispatch based on type
        if (type === 'symbols-ready' && typeof window.onSymbolsNotify === 'function') {
          window.onSymbolsNotify();
        }
        if (type === 'candles-ready' && typeof window.onCandlesNotify === 'function') {
          window.onCandlesNotify(msg);
        }
      } catch (_) {
        // ignore malformed
      }
    };
    notifyWS.onclose = () => { notifyWS = null; };
    notifyWS.onerror = () => { notifyWS = null; };
  } catch (_) {
    notifyWS = null;
  }
}
