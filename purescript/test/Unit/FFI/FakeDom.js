const makeElement = (tagName) => {
  const el = {
    tagName: tagName.toUpperCase(),
    attributes: {},
    children: [],
    parentNode: null,
    textContent: "",
    _listeners: {},
    setAttribute(name, value) {
      this.attributes[name] = String(value);
    },
    appendChild(child) {
      child.parentNode = this;
      this.children.push(child);
      return child;
    },
    addEventListener(type, listener) {
      if (!this._listeners[type]) this._listeners[type] = [];
      this._listeners[type].push(listener);
    },
    dispatchEventType(type) {
      const ev = {
        type,
        target: this,
        currentTarget: this,
        _stopped: false,
        stopPropagation() {
          this._stopped = true;
        }
      };
      let cur = this;
      while (cur) {
        ev.currentTarget = cur;
        const ls = cur._listeners[type] || [];
        for (const l of ls) {
          const ret = l(ev);
          if (typeof ret === "function") ret();
        }
        if (ev._stopped) break;
        cur = cur.parentNode;
      }
    }
  };
  return el;
};

const makeDocument = () => ({
  createElement(tagName) {
    return makeElement(tagName);
  }
});

const findFirstByTag = (root, tag) => {
  if (!root) return null;
  if ((root.tagName || "").toLowerCase() === tag.toLowerCase()) return root;
  for (const child of root.children || []) {
    const found = findFirstByTag(child, tag);
    if (found) return found;
  }
  return null;
};

export const installFakeDom = () => {
  const doc = makeDocument();
  globalThis.window = { document: doc };
  globalThis.document = doc;
};

export const clickFirstSpan = (root) => () => {
  const span = findFirstByTag(root, "span");
  if (span) span.dispatchEventType("click");
};
