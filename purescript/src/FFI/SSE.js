export const attachEventSource = (route) => (handler) => () => {
  const es = new EventSource(route);
  es.onmessage = () => {
    handler(undefined)();
  };
  es.onerror = () => {
    // Keep connection alive; browser will retry automatically.
  };
};
