// data.js: FRP for Data page (symbol counts)

// Require RxJS UMD loaded via Layout
const symbols$ =
  (typeof rxjs !== 'undefined' && rxjs.BehaviorSubject)
    ? new rxjs.BehaviorSubject({ status: 'idle', data: null, error: null })
    : null;

function requestSymbols(skipCheck = false) {
  if (!symbols$) return;
  const prev = symbols$.getValue();
  symbols$.next({ status: 'loading', data: prev.data, error: null });
  const qs = skipCheck ? '?skipCheck=true' : '';
  fetch('/api/symbols' + qs)
    .then((r) => r.json())
    .then((data) => symbols$.next({ status: 'ready', data, error: null }))
    .catch((err) => symbols$.next({ status: 'error', data: null, error: err }));
}

function hydrateSymbolCounts() {
  if (!symbols$) return;
  const els = document.querySelectorAll('[data-frp-symbol-count]');
  if (!els.length) return;

  symbols$.subscribe((state) => {
    els.forEach((el) => {
      const st = el.dataset.symbolType;
      const label = (el.dataset.label || el.textContent.split(':')[0] || st).trim();
      if (state.status === 'loading') {
        el.textContent = `${label}: ...`;
      } else if (state.status === 'error') {
        el.textContent = `${label}: (err)`;
      } else if (state.status === 'ready' && state.data) {
        const val = state.data[st] ?? 0;
        el.textContent = `${label}: ${val}`;
      }
    });
  });

  requestSymbols();
}

// Public init for Data page
window.dataInit = function dataInit() {
  hydrateSymbolCounts();
};

// Optional hook for WS notify
window.onSymbolsNotify = function onSymbolsNotify() {
  requestSymbols(true);
};

// Candle notify hook placeholder; concrete implementation should refetch with skipCheck=true
window.onCandlesNotify = function onCandlesNotify(_msg) {
  // Implement per-chart refetch in your chart module; keeping stub here.
};
