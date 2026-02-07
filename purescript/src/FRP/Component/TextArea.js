export const autoResize = (textarea) => () => {
  if (!textarea) return;
  const apply = () => {
    textarea.style.overflow = "hidden";
    textarea.style.resize = "none";
    textarea.style.height = "auto";
    textarea.style.height = `${textarea.scrollHeight}px`;
  };

  apply();
  if (typeof window !== "undefined" && typeof window.requestAnimationFrame === "function") {
    window.requestAnimationFrame(apply);
    window.requestAnimationFrame(() => window.requestAnimationFrame(apply));
  }
};

export const autoResizeDeferred = (textarea) => () => {
  if (!textarea) return;
  setTimeout(() => {
    textarea.style.overflow = "hidden";
    textarea.style.resize = "none";
    textarea.style.height = "auto";
    textarea.style.height = `${textarea.scrollHeight}px`;
  }, 0);
};
