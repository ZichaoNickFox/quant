import { Just, Nothing } from "../Data.Maybe/index.js";

export const setInnerHTMLById = (id) => (html) => () => {
  const el = globalThis.document.getElementById(id);
  if (!el) return false;
  el.innerHTML = html;
  return true;
};

export const getQueryParam = (key) => () => {
  const search = globalThis?.window?.location?.search;
  if (typeof search !== "string") return Nothing.value;
  const params = new URLSearchParams(search);
  const value = params.get(key);
  return value === null ? Nothing.value : Just.create(value);
};
