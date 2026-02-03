export const attachEventSource = (route) => (handler) => () => {
  const es = new EventSource(route);
  es.onmessage = (ev) => {
    handler(ev.data)();
  };
  es.onerror = () => {
    // Keep connection alive; browser will retry automatically.
  };
};
