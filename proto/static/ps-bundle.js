(() => {
  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var on = function(f) {
    return function(g) {
      return function(x) {
        return function(y) {
          return f(g(x))(g(y));
        };
      };
    };
  };
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map12 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map12(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map12 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map12($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var when = function(dictApplicative) {
    var pure12 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure12(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply3(pure12(f))(a);
      };
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce = function(x) {
    return x;
  };

  // output/Control.Monad.ST.Global/index.js
  var toEffect = unsafeCoerce;

  // output/Control.Monad.ST.Internal/foreign.js
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };

  // output/Control.Bind/foreign.js
  var arrayBind = typeof Array.prototype.flatMap === "function" ? function(arr) {
    return function(f) {
      return arr.flatMap(f);
    };
  } : function(arr) {
    return function(f) {
      var result = [];
      var l = arr.length;
      for (var i = 0; i < l; i++) {
        var xs = f(arr[i]);
        var k = xs.length;
        for (var j = 0; j < k; j++) {
          result.push(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped1 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bindFlipped1(f)(g(a));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind12 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bind12(f(a))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind6 = bind(dictMonad.Bind1());
    var pure7 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind6(f)(function(f$prime) {
          return bind6(a)(function(a$prime) {
            return pure7(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label4) {
    return function(rec) {
      return rec[label4];
    };
  };
  var unsafeSet = function(label4) {
    return function(value13) {
      return function(rec) {
        var copy = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }
        copy[label4] = value13;
        return copy;
      };
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty4 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty4;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showUnit = {
    show: function(v) {
      return "unit";
    }
  };
  var showString = {
    show: showStringImpl
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showInt = {
    show: showIntImpl
  };
  var showBoolean = {
    show: function(v) {
      if (v) {
        return "true";
      }
      ;
      if (!v) {
        return "false";
      }
      ;
      throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
    }
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show13 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key = reflectSymbol2($$Proxy.value);
              var focus2 = unsafeGet(key)(record);
              return " " + (key + (": " + (show13(focus2) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show13 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key = reflectSymbol2($$Proxy.value);
            var focus2 = unsafeGet(key)(record);
            return " " + (key + (": " + (show13(focus2) + " ")));
          };
        }
      };
    };
  };

  // output/Data.Generic.Rep/index.js
  var Argument = function(x) {
    return x;
  };
  var to = function(dict) {
    return dict.to;
  };
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var showEither = function(dictShow) {
    var show7 = show(dictShow);
    return function(dictShow1) {
      var show13 = show(dictShow1);
      return {
        show: function(v) {
          if (v instanceof Left) {
            return "(Left " + (show7(v.value0) + ")");
          }
          ;
          if (v instanceof Right) {
            return "(Right " + (show13(v.value0) + ")");
          }
          ;
          throw new Error("Failed pattern match at Data.Either (line 173, column 1 - line 175, column 46): " + [v.constructor.name]);
        }
      };
    };
  };
  var note = function(a) {
    return maybe(new Left(a))(Right.create);
  };
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a) {
      return function(f) {
        return f(a);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Control.Monad.ST.Internal/index.js
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };

  // output/Control.Monad.ST.Class/index.js
  var monadSTEffect = {
    liftST: toEffect,
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftST = function(dict) {
    return dict.liftST;
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count2, value13) {
    if (count2 < 1) {
      return [];
    }
    var result = new Array(count2);
    return result.fill(value13);
  };
  var replicatePolyfill = function(count2, value13) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count2; i++) {
      result[n++] = value13;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty4, next, xs) {
    return xs.length === 0 ? empty4({}) : next(xs[0])(xs.slice(1));
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (f(xs[i])) return just(i);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i, l) {
    if (i < 0 || i >= l.length) return nothing;
    var l1 = l.slice();
    l1.splice(i, 1);
    return just(l1);
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };

  // output/Data.Array.ST/foreign.js
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  function copyImpl(xs) {
    return xs.slice();
  }
  var thawImpl = copyImpl;
  var pushImpl = function(a, xs) {
    return xs.push(a);
  };

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
  var withArray = function(f) {
    return function(xs) {
      return function __do3() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = /* @__PURE__ */ runSTFn2(pushImpl);

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var not = function(dict) {
    return dict.not;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();

  // output/Data.Bifunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity4);
    };
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 38, column 1 - line 40, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x) {
    return x;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure7 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure7(unit));
      };
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append3 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append3(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var any = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
    };
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a) {
      return function(b) {
        return fn(a, b);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return fn(a, b, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map12) {
        return function(pure7) {
          return function(f) {
            return function(array) {
              function go2(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure7([]);
                  case 1:
                    return map12(array1)(f(array[bot]));
                  case 2:
                    return apply3(map12(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map12(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply3(map12(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity5);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Array/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var uncons = /* @__PURE__ */ function() {
    return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i) {
          return fromJust2(deleteAt(i)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $189 = maybe([])(singleton2);
      return function($190) {
        return $189(f($190));
      };
    }());
  };

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append3 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i) {
          return function(x) {
            return function(acc) {
              return append3(f(i)(x))(acc);
            };
          };
        })(mempty2);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $291 = foldr8(function(v) {
          return function(y) {
            return f(v.value0)(v.value1)(y);
          };
        })(z);
        var $292 = mapWithIndex2(Tuple.create);
        return function($293) {
          return $291($292($293));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $294 = foldl8(function(y) {
          return function(v) {
            return f(v.value0)(y)(v.value1);
          };
        })(z);
        var $295 = mapWithIndex2(Tuple.create);
        return function($296) {
          return $294($295($296));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function() {
      return foldableArray;
    }
  };

  // output/Data.TraversableWithIndex/index.js
  var traverseWithIndexDefault = function(dictTraversableWithIndex) {
    var sequence2 = sequence(dictTraversableWithIndex.Traversable2());
    var mapWithIndex4 = mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
    return function(dictApplicative) {
      var sequence12 = sequence2(dictApplicative);
      return function(f) {
        var $174 = mapWithIndex4(f);
        return function($175) {
          return sequence12($174($175));
        };
      };
    };
  };
  var traverseWithIndex = function(dict) {
    return dict.traverseWithIndex;
  };
  var traversableWithIndexArray = {
    traverseWithIndex: function(dictApplicative) {
      return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
    },
    FunctorWithIndex0: function() {
      return functorWithIndexArray;
    },
    FoldableWithIndex1: function() {
      return foldableWithIndexArray;
    },
    Traversable2: function() {
      return traversableArray;
    }
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton3 = function(dictPlus) {
    var empty4 = empty(dictPlus);
    return function(a) {
      return new NonEmpty(a, empty4);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil2() {
    }
    ;
    Nil2.value = new Nil2();
    return Nil2;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons2.create = function(value0) {
      return function(value1) {
        return new Cons2(value0, value1);
      };
    };
    return Cons2;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a) {
    return function(b) {
      return a === b;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/FRP.Event/index.js
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var subscribe = function(v) {
    return function(k) {
      return v(k);
    };
  };
  var create = function(dictMonadST) {
    var Monad0 = dictMonadST.Monad0();
    var bind12 = bind(Monad0.Bind1());
    var liftST2 = liftST(dictMonadST);
    var pure12 = pure(Monad0.Applicative0());
    return function(dictMonadST1) {
      var Monad01 = dictMonadST1.Monad0();
      var bind22 = bind(Monad01.Bind1());
      var liftST1 = liftST(dictMonadST1);
      var Applicative0 = Monad01.Applicative0();
      var pure22 = pure(Applicative0);
      var traverse_3 = traverse_(Applicative0)(foldableArray);
      return bind12(liftST2(newSTRef([])))(function(subscribers) {
        return pure12({
          event: function(k) {
            return bind22(liftST1(modify(function(v) {
              return append12(v)([k]);
            })(subscribers)))(function() {
              return pure22(bind22(liftST1(modify(deleteBy(unsafeRefEq)(k))(subscribers)))(function() {
                return pure22(unit);
              }));
            });
          },
          push: function(a) {
            return bind22(liftST1(read(subscribers)))(traverse_3(function(k) {
              return k(a);
            }));
          }
        });
      });
    };
  };

  // output/FRP.Log/index.js
  var subscribeWithLog = function(dictShow) {
    var show7 = show(dictShow);
    return function(event) {
      return function(label4) {
        return function(handler) {
          return subscribe(event)(function(a) {
            return function __do3() {
              log("[subs]" + (label4 + (" " + show7(a))))();
              return handler(a)();
            };
          });
        };
      };
    };
  };
  var pushWithLogUnit = function(push2) {
    return function(label4) {
      return function __do3() {
        log("[push]" + label4)();
        return push2(unit)();
      };
    };
  };
  var pushWithLog = function(dictShow) {
    var show7 = show(dictShow);
    return function(push2) {
      return function(label4) {
        return function(value13) {
          return function __do3() {
            log("[push]" + (label4 + (" " + show7(value13))))();
            return push2(value13)();
          };
        };
      };
    };
  };

  // output/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options2) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options2.url, xhr);
      xhr.open(options2.method || "GET", fixedUrl, true, options2.username, options2.password);
      if (options2.headers) {
        try {
          for (var i = 0, header; (header = options2.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function(msgIdent) {
        return function() {
          errback(new Error(msgIdent));
        };
      };
      xhr.onerror = onerror(requestFailedMessageIdent);
      xhr.ontimeout = onerror(timeoutErrorMessageIdent);
      xhr.onload = function() {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header2) {
            return header2.length > 0;
          }).map(function(header2) {
            var i2 = header2.indexOf(":");
            return mkHeader(header2.substring(0, i2))(header2.substring(i2 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options2.responseType;
      xhr.withCredentials = options2.withCredentials;
      xhr.timeout = options2.timeout;
      xhr.send(options2.content);
      return function(error3, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  }

  // output/Data.MediaType.Common/index.js
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";

  // output/Affjax.RequestBody/index.js
  var ArrayView = /* @__PURE__ */ function() {
    function ArrayView2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayView2.create = function(value0) {
      return new ArrayView2(value0);
    };
    return ArrayView2;
  }();
  var Blob = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var $$String = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var FormData = /* @__PURE__ */ function() {
    function FormData2(value0) {
      this.value0 = value0;
    }
    ;
    FormData2.create = function(value0) {
      return new FormData2(value0);
    };
    return FormData2;
  }();
  var FormURLEncoded = /* @__PURE__ */ function() {
    function FormURLEncoded2(value0) {
      this.value0 = value0;
    }
    ;
    FormURLEncoded2.create = function(value0) {
      return new FormURLEncoded2(value0);
    };
    return FormURLEncoded2;
  }();
  var Json = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
  var toMediaType = function(v) {
    if (v instanceof FormURLEncoded) {
      return new Just(applicationFormURLEncoded);
    }
    ;
    if (v instanceof Json) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };

  // output/Affjax.RequestHeader/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var Accept = /* @__PURE__ */ function() {
    function Accept2(value0) {
      this.value0 = value0;
    }
    ;
    Accept2.create = function(value0) {
      return new Accept2(value0);
    };
    return Accept2;
  }();
  var ContentType = /* @__PURE__ */ function() {
    function ContentType2(value0) {
      this.value0 = value0;
    }
    ;
    ContentType2.create = function(value0) {
      return new ContentType2(value0);
    };
    return ContentType2;
  }();
  var RequestHeader = /* @__PURE__ */ function() {
    function RequestHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RequestHeader2.create = function(value0) {
      return function(value1) {
        return new RequestHeader2(value0, value1);
      };
    };
    return RequestHeader2;
  }();
  var value = function(v) {
    if (v instanceof Accept) {
      return unwrap2(v.value0);
    }
    ;
    if (v instanceof ContentType) {
      return unwrap2(v.value0);
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };
  var name = function(v) {
    if (v instanceof Accept) {
      return "Accept";
    }
    ;
    if (v instanceof ContentType) {
      return "Content-Type";
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  // output/Affjax.ResponseFormat/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var $$ArrayBuffer = /* @__PURE__ */ function() {
    function $$ArrayBuffer2(value0) {
      this.value0 = value0;
    }
    ;
    $$ArrayBuffer2.create = function(value0) {
      return new $$ArrayBuffer2(value0);
    };
    return $$ArrayBuffer2;
  }();
  var Blob2 = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document2 = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var Json2 = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
  var $$String2 = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var Ignore = /* @__PURE__ */ function() {
    function Ignore2(value0) {
      this.value0 = value0;
    }
    ;
    Ignore2.create = function(value0) {
      return new Ignore2(value0);
    };
    return Ignore2;
  }();
  var toResponseType = function(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }
    ;
    if (v instanceof Blob2) {
      return "blob";
    }
    ;
    if (v instanceof Document2) {
      return "document";
    }
    ;
    if (v instanceof Json2) {
      return "text";
    }
    ;
    if (v instanceof $$String2) {
      return "text";
    }
    ;
    if (v instanceof Ignore) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 44, column 3 - line 50, column 19): " + [v.constructor.name]);
  };
  var toMediaType2 = function(v) {
    if (v instanceof Json2) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };
  var json = /* @__PURE__ */ function() {
    return new Json2(identity6);
  }();
  var ignore = /* @__PURE__ */ function() {
    return new Ignore(identity6);
  }();

  // output/Affjax.ResponseHeader/index.js
  var ResponseHeader = /* @__PURE__ */ function() {
    function ResponseHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseHeader2.create = function(value0) {
      return function(value1) {
        return new ResponseHeader2(value0, value1);
      };
    };
    return ResponseHeader2;
  }();

  // output/Effect.Exception/foreign.js
  function message(e) {
    return e.message;
  }

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map12 = map(Monad0.Bind1().Apply0().Functor0());
    var pure7 = pure(Monad0.Applicative0());
    return function(a) {
      return catchError1(map12(Right.create)(a))(function($52) {
        return pure7(Left.create($52));
      });
    };
  };

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map12 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map12(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind6 = bind(dictMonad.Bind1());
    var pure7 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind6(v)(either(function($193) {
            return pure7(Left.create($193));
          })(function(a) {
            var v1 = k(a);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $194 = pure(dictMonad.Applicative0());
        return function($195) {
          return ExceptT($194(Right.create($195)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $204 = pure(dictMonad.Applicative0());
        return function($205) {
          return ExceptT($204(Left.create($205)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append3 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind6 = bind(Bind1);
      var pure7 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind6(v)(function(rm) {
              if (rm instanceof Right) {
                return pure7(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind6(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure7(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure7(new Left(append3(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 87, column 9 - line 89, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 83, column 5 - line 89, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Monad.Except/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap3(runExceptT($3));
  };

  // output/Data.Argonaut.Core/foreign.js
  function id(x) {
    return x;
  }
  var jsonNull = null;
  function stringify(j) {
    return JSON.stringify(j);
  }
  function _caseJson(isNull3, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null) return isNull3();
    else if (typeof j === "boolean") return isBool(j);
    else if (typeof j === "number") return isNum(j);
    else if (typeof j === "string") return isStr(j);
    else if (Object.prototype.toString.call(j) === "[object Array]")
      return isArr(j);
    else return isObj(j);
  }

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty3 = {};
  function runST(f) {
    return f();
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object.ST/foreign.js
  var deleteImpl = function(k) {
    return function(m) {
      return function() {
        delete m[k];
        return m;
      };
    };
  };

  // output/Foreign.Object/index.js
  var thawST = _copyST;
  var mutate = function(f) {
    return function(m) {
      return runST(function __do3() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var lookup = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();
  var $$delete3 = function(k) {
    return mutate(deleteImpl(k));
  };

  // output/Data.Argonaut.Core/index.js
  var verbJsonType = function(def) {
    return function(f) {
      return function(g) {
        return g(def)(f);
      };
    };
  };
  var toJsonType = /* @__PURE__ */ function() {
    return verbJsonType(Nothing.value)(Just.create);
  }();
  var jsonEmptyObject = /* @__PURE__ */ id(empty3);
  var caseJsonString = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), f, $$const(d), $$const(d), j);
      };
    };
  };
  var toString = /* @__PURE__ */ toJsonType(caseJsonString);
  var caseJsonObject = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), $$const(d), f, j);
      };
    };
  };
  var toObject = /* @__PURE__ */ toJsonType(caseJsonObject);
  var caseJsonBoolean = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), f, $$const(d), $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var caseJsonArray = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), f, $$const(d), j);
      };
    };
  };
  var toArray = /* @__PURE__ */ toJsonType(caseJsonArray);
  var caseJson = function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(json2) {
                return _caseJson(a, b, c, d, e, f, json2);
              };
            };
          };
        };
      };
    };
  };

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail3, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail3(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/Data.String.Common/foreign.js
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/JSURI/foreign.js
  function encodeURIComponent_to_RFC3986(input) {
    return input.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  function _encodeFormURLComponent(fail3, succeed, input) {
    try {
      return succeed(encodeURIComponent_to_RFC3986(encodeURIComponent(input)).replace(/%20/g, "+"));
    } catch (err) {
      return fail3(err);
    }
  }

  // output/JSURI/index.js
  var encodeFormURLComponent = /* @__PURE__ */ function() {
    return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
  }();

  // output/Data.FormURLEncoded/index.js
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
  var toArray2 = function(v) {
    return v;
  };
  var encode = /* @__PURE__ */ function() {
    var encodePart = function(v) {
      if (v.value1 instanceof Nothing) {
        return encodeFormURLComponent(v.value0);
      }
      ;
      if (v.value1 instanceof Just) {
        return apply2(map5(function(key) {
          return function(val) {
            return key + ("=" + val);
          };
        })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
    };
    var $37 = map5(joinWith("&"));
    var $38 = traverse2(encodePart);
    return function($39) {
      return $37($38(toArray2($39)));
    };
  }();

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  }();
  var GET = /* @__PURE__ */ function() {
    function GET2() {
    }
    ;
    GET2.value = new GET2();
    return GET2;
  }();
  var HEAD = /* @__PURE__ */ function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  }();
  var POST = /* @__PURE__ */ function() {
    function POST2() {
    }
    ;
    POST2.value = new POST2();
    return POST2;
  }();
  var PUT = /* @__PURE__ */ function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  }();
  var DELETE = /* @__PURE__ */ function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  }();
  var TRACE = /* @__PURE__ */ function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  }();
  var CONNECT = /* @__PURE__ */ function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  }();
  var PROPFIND = /* @__PURE__ */ function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  }();
  var PROPPATCH = /* @__PURE__ */ function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  }();
  var MKCOL = /* @__PURE__ */ function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  }();
  var COPY = /* @__PURE__ */ function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  }();
  var MOVE = /* @__PURE__ */ function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  }();
  var LOCK = /* @__PURE__ */ function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  }();
  var UNLOCK = /* @__PURE__ */ function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  }();
  var PATCH = /* @__PURE__ */ function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  }();
  var unCustomMethod = function(v) {
    return v;
  };
  var showMethod = {
    show: function(v) {
      if (v instanceof OPTIONS) {
        return "OPTIONS";
      }
      ;
      if (v instanceof GET) {
        return "GET";
      }
      ;
      if (v instanceof HEAD) {
        return "HEAD";
      }
      ;
      if (v instanceof POST) {
        return "POST";
      }
      ;
      if (v instanceof PUT) {
        return "PUT";
      }
      ;
      if (v instanceof DELETE) {
        return "DELETE";
      }
      ;
      if (v instanceof TRACE) {
        return "TRACE";
      }
      ;
      if (v instanceof CONNECT) {
        return "CONNECT";
      }
      ;
      if (v instanceof PROPFIND) {
        return "PROPFIND";
      }
      ;
      if (v instanceof PROPPATCH) {
        return "PROPPATCH";
      }
      ;
      if (v instanceof MKCOL) {
        return "MKCOL";
      }
      ;
      if (v instanceof COPY) {
        return "COPY";
      }
      ;
      if (v instanceof MOVE) {
        return "MOVE";
      }
      ;
      if (v instanceof LOCK) {
        return "LOCK";
      }
      ;
      if (v instanceof UNLOCK) {
        return "UNLOCK";
      }
      ;
      if (v instanceof PATCH) {
        return "PATCH";
      }
      ;
      throw new Error("Failed pattern match at Data.HTTP.Method (line 43, column 1 - line 59, column 23): " + [v.constructor.name]);
    }
  };
  var print = /* @__PURE__ */ either(/* @__PURE__ */ show(showMethod))(unCustomMethod);

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.List.NonEmpty/index.js
  var singleton7 = /* @__PURE__ */ function() {
    var $200 = singleton3(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var head = function(v) {
    return v.value0;
  };

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error3) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error3) {
        setTimeout(function() {
          throw error3;
        }, 0);
      }
    }
    function runSync(left2, right2, eff) {
      try {
        return right2(eff());
      } catch (error3) {
        return left2(error3);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error3) {
        k(left2(error3))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count2 = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count2--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count2++;
        },
        isEmpty: function() {
          return count2 === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count2 === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count2 = 0;
            return function(error3) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step2 = aff;
      var fail3 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step2 = bhead(step2);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util.left(e);
                step2 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step2)) {
                status = RETURN;
                fail3 = step2;
                step2 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step2 = util.fromRight(step2);
              }
              break;
            case CONTINUE:
              switch (step2.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step2._2;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step2 = util.right(step2._1);
                  } else {
                    status = STEP_BIND;
                    step2 = step2._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step2 = runSync(util.left, util.right, step2._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step2 = runAsync(util.left, step2._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step2 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util.left(step2._1);
                  step2 = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step2._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step2._1) {
                    tmp.run();
                  }
                  step2 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step2 = sequential2(util, supervisor, step2._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step2 = interrupt || fail3 || step2;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step2 = attempt._2(util.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step2 = util.fromRight(step2);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util.fromRight(step2);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step2 = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step2 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step2 = attempt._1.failed(util.fromLeft(fail3))(attempt._2);
                    } else {
                      step2 = attempt._1.completed(util.fromRight(step2))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step2 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step2 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step2));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util.fromLeft(fail3);
                }, 0);
              } else if (util.isLeft(step2) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step2);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join3) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join3.rethrow;
            join3.handler(step2)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join3;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error3, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error3);
              status = COMPLETED;
              step2 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error3)), attempts, interrupt);
                }
                status = RETURN;
                step2 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step2 = null;
                fail3 = null;
              }
          }
          return canceler;
        };
      }
      function join2(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join2,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill(error3, par2, cb2) {
        var step2 = par2;
        var head3 = null;
        var tail2 = null;
        var count2 = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step2.tag) {
            case FORKED:
              if (step2._3 === EMPTY) {
                tmp = fibers[step2._1];
                kills2[count2++] = tmp.kill(error3, function(result) {
                  return function() {
                    count2--;
                    if (count2 === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head3 === null) {
                break loop;
              }
              step2 = head3._2;
              if (tail2 === null) {
                head3 = null;
              } else {
                head3 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step2 = step2._2;
              break;
            case APPLY:
            case ALT:
              if (head3) {
                tail2 = new Aff2(CONS, head3, tail2);
              }
              head3 = step2;
              step2 = step2._1;
              break;
          }
        }
        if (count2 === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count2;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join2(result, head3, tail2) {
        var fail3, step2, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail3 = result;
          step2 = null;
        } else {
          step2 = result;
          fail3 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head3 === null) {
            cb(fail3 || step2)();
            return;
          }
          if (head3._3 !== EMPTY) {
            return;
          }
          switch (head3.tag) {
            case MAP:
              if (fail3 === null) {
                head3._3 = util.right(head3._1(util.fromRight(step2)));
                step2 = head3._3;
              } else {
                head3._3 = fail3;
              }
              break;
            case APPLY:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (fail3) {
                head3._3 = fail3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail3 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join2(fail3, null, null);
                    } else {
                      join2(fail3, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step2 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head3._3 = step2;
              }
              break;
            case ALT:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail3 = step2 === lhs ? rhs : lhs;
                step2 = null;
                head3._3 = fail3;
              } else {
                head3._3 = step2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step2 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join2(step2, null, null);
                    } else {
                      join2(step2, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail2 === null) {
            head3 = null;
          } else {
            head3 = tail2._1;
            tail2 = tail2._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join2(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step2 = par;
        var head3 = null;
        var tail2 = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step2.tag) {
                case MAP:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                  step2 = step2._2;
                  break;
                case APPLY:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                case ALT:
                  if (head3) {
                    tail2 = new Aff2(CONS, head3, tail2);
                  }
                  head3 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step2;
                  step2 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail2), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step2)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head3 === null) {
                break loop;
              }
              if (head3._1 === EMPTY) {
                head3._1 = step2;
                status = CONTINUE;
                step2 = head3._2;
                head3._2 = EMPTY;
              } else {
                head3._2 = step2;
                step2 = head3;
                if (tail2 === null) {
                  head3 = null;
                } else {
                  head3 = tail2._1;
                  tail2 = tail2._2;
                }
              }
          }
        }
        root = step2;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error3, cb2) {
        interrupt = util.left(error3);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error3, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value13) {
          return Aff.Pure(f(value13));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var functorAff = {
    map: _map
  };
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do3() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var launchAff_ = function($75) {
    return $$void2(launchAff($75));
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var pure2 = /* @__PURE__ */ pure(applicativeAff);
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure2(unit));

  // output/Effect.Aff.Compat/index.js
  var fromEffectFnAff = function(v) {
    return makeAff(function(k) {
      return function __do3() {
        var v1 = v(function($9) {
          return k(Left.create($9))();
        }, function($10) {
          return k(Right.create($10))();
        });
        return function(e) {
          return makeAff(function(k2) {
            return function __do4() {
              v1(e, function($11) {
                return k2(Left.create($11))();
              }, function($12) {
                return k2(Right.create($12))();
              });
              return nonCanceler;
            };
          });
        };
      };
    });
  };

  // output/Foreign/foreign.js
  function tagOf(value13) {
    return Object.prototype.toString.call(value13).slice(8, -1);
  }
  var isArray = Array.isArray || function(value13) {
    return Object.prototype.toString.call(value13) === "[object Array]";
  };

  // output/Foreign/index.js
  var show2 = /* @__PURE__ */ show(showString);
  var show1 = /* @__PURE__ */ show(showInt);
  var ForeignError = /* @__PURE__ */ function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  }();
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return function(value1) {
        return new TypeMismatch3(value0, value1);
      };
    };
    return TypeMismatch3;
  }();
  var ErrorAtIndex = /* @__PURE__ */ function() {
    function ErrorAtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtIndex2.create = function(value0) {
      return function(value1) {
        return new ErrorAtIndex2(value0, value1);
      };
    };
    return ErrorAtIndex2;
  }();
  var ErrorAtProperty = /* @__PURE__ */ function() {
    function ErrorAtProperty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtProperty2.create = function(value0) {
      return function(value1) {
        return new ErrorAtProperty2(value0, value1);
      };
    };
    return ErrorAtProperty2;
  }();
  var unsafeToForeign = unsafeCoerce;
  var unsafeFromForeign = unsafeCoerce;
  var renderForeignError = function(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }
    ;
    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (show1(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (show2(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }
    ;
    throw new Error("Failed pattern match at Foreign (line 78, column 1 - line 78, column 45): " + [v.constructor.name]);
  };
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton7($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure12 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value13) {
        if (tagOf(value13) === tag) {
          return pure12(unsafeFromForeign(value13));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value13)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value13.constructor.name]);
      };
    };
  };

  // output/Affjax/index.js
  var pure3 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail2 = /* @__PURE__ */ fail(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map6 = /* @__PURE__ */ map(functorMaybe);
  var any2 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var eq2 = /* @__PURE__ */ eq(eqString);
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map1 = /* @__PURE__ */ map(functorArray);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorAff);
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var pure1 = /* @__PURE__ */ pure(applicativeAff);
  var RequestContentError = /* @__PURE__ */ function() {
    function RequestContentError2(value0) {
      this.value0 = value0;
    }
    ;
    RequestContentError2.create = function(value0) {
      return new RequestContentError2(value0);
    };
    return RequestContentError2;
  }();
  var ResponseBodyError = /* @__PURE__ */ function() {
    function ResponseBodyError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseBodyError2.create = function(value0) {
      return function(value1) {
        return new ResponseBodyError2(value0, value1);
      };
    };
    return ResponseBodyError2;
  }();
  var TimeoutError = /* @__PURE__ */ function() {
    function TimeoutError2() {
    }
    ;
    TimeoutError2.value = new TimeoutError2();
    return TimeoutError2;
  }();
  var RequestFailedError = /* @__PURE__ */ function() {
    function RequestFailedError2() {
    }
    ;
    RequestFailedError2.value = new RequestFailedError2();
    return RequestFailedError2;
  }();
  var XHROtherError = /* @__PURE__ */ function() {
    function XHROtherError2(value0) {
      this.value0 = value0;
    }
    ;
    XHROtherError2.create = function(value0) {
      return new XHROtherError2(value0);
    };
    return XHROtherError2;
  }();
  var request = function(driver2) {
    return function(req) {
      var parseJSON = function(v2) {
        if (v2 === "") {
          return pure3(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail2(ForeignError.create($74));
        })(pure3)(jsonParser(v2));
      };
      var fromResponse = function() {
        if (req.responseFormat instanceof $$ArrayBuffer) {
          return unsafeReadTagged2("ArrayBuffer");
        }
        ;
        if (req.responseFormat instanceof Blob2) {
          return unsafeReadTagged2("Blob");
        }
        ;
        if (req.responseFormat instanceof Document2) {
          return function(x) {
            return alt2(unsafeReadTagged2("Document")(x))(alt2(unsafeReadTagged2("XMLDocument")(x))(unsafeReadTagged2("HTMLDocument")(x)));
          };
        }
        ;
        if (req.responseFormat instanceof Json2) {
          return composeKleisliFlipped2(function($75) {
            return req.responseFormat.value0(parseJSON($75));
          })(unsafeReadTagged2("String"));
        }
        ;
        if (req.responseFormat instanceof $$String2) {
          return unsafeReadTagged2("String");
        }
        ;
        if (req.responseFormat instanceof Ignore) {
          return $$const(req.responseFormat.value0(pure3(unit)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req.responseFormat.constructor.name]);
      }();
      var extractContent = function(v2) {
        if (v2 instanceof ArrayView) {
          return new Right(v2.value0(unsafeToForeign));
        }
        ;
        if (v2 instanceof Blob) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof Document) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof $$String) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormData) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormURLEncoded) {
          return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map6(unsafeToForeign)(encode(v2.value0)));
        }
        ;
        if (v2 instanceof Json) {
          return new Right(unsafeToForeign(stringify(v2.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 235, column 20 - line 250, column 69): " + [v2.constructor.name]);
      };
      var addHeader = function(mh) {
        return function(hs) {
          if (mh instanceof Just && !any2(on(eq2)(name)(mh.value0))(hs)) {
            return snoc(hs)(mh.value0);
          }
          ;
          return hs;
        };
      };
      var headers = function(reqContent) {
        return addHeader(map6(ContentType.create)(bindFlipped2(toMediaType)(reqContent)))(addHeader(map6(Accept.create)(toMediaType2(req.responseFormat)))(req.headers));
      };
      var ajaxRequest = function(v2) {
        return {
          method: print(req.method),
          url: req.url,
          headers: map1(function(h) {
            return {
              field: name(h),
              value: value(h)
            };
          })(headers(req.content)),
          content: v2,
          responseType: toResponseType(req.responseFormat),
          username: toNullable(req.username),
          password: toNullable(req.password),
          withCredentials: req.withCredentials,
          timeout: fromMaybe(0)(map6(function(v1) {
            return v1;
          })(req.timeout))
        };
      };
      var send = function(content3) {
        return mapFlipped2($$try2(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content3)))))(function(v2) {
          if (v2 instanceof Right) {
            var v1 = runExcept(fromResponse(v2.value0.body));
            if (v1 instanceof Left) {
              return new Left(new ResponseBodyError(head(v1.value0), v2.value0));
            }
            ;
            if (v1 instanceof Right) {
              return new Right({
                headers: v2.value0.headers,
                status: v2.value0.status,
                statusText: v2.value0.statusText,
                body: v1.value0
              });
            }
            ;
            throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
          }
          ;
          if (v2 instanceof Left) {
            return new Left(function() {
              var message2 = message(v2.value0);
              var $61 = message2 === "AffjaxTimeoutErrorMessageIdent";
              if ($61) {
                return TimeoutError.value;
              }
              ;
              var $62 = message2 === "AffjaxRequestFailedMessageIdent";
              if ($62) {
                return RequestFailedError.value;
              }
              ;
              return new XHROtherError(v2.value0);
            }());
          }
          ;
          throw new Error("Failed pattern match at Affjax (line 207, column 144 - line 219, column 28): " + [v2.constructor.name]);
        });
      };
      if (req.content instanceof Nothing) {
        return send(toNullable(Nothing.value));
      }
      ;
      if (req.content instanceof Just) {
        var v = extractContent(req.content.value0);
        if (v instanceof Right) {
          return send(toNullable(new Just(v.value0)));
        }
        ;
        if (v instanceof Left) {
          return pure1(new Left(new RequestContentError(v.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 199, column 7 - line 203, column 48): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 195, column 3 - line 203, column 48): " + [req.content.constructor.name]);
    };
  };
  var printError = function(v) {
    if (v instanceof RequestContentError) {
      return "There was a problem with the request content: " + v.value0;
    }
    ;
    if (v instanceof ResponseBodyError) {
      return "There was a problem with the response body: " + renderForeignError(v.value0);
    }
    ;
    if (v instanceof TimeoutError) {
      return "There was a problem making the request: timeout";
    }
    ;
    if (v instanceof RequestFailedError) {
      return "There was a problem making the request: request failed";
    }
    ;
    if (v instanceof XHROtherError) {
      return "There was a problem making the request: " + message(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Affjax (line 113, column 14 - line 123, column 66): " + [v.constructor.name]);
  };
  var defaultRequest = /* @__PURE__ */ function() {
    return {
      method: new Left(GET.value),
      url: "/",
      headers: [],
      content: Nothing.value,
      username: Nothing.value,
      password: Nothing.value,
      withCredentials: false,
      responseFormat: ignore,
      timeout: Nothing.value
    };
  }();
  var get = function(driver2) {
    return function(rf) {
      return function(u) {
        return request(driver2)({
          method: defaultRequest.method,
          headers: defaultRequest.headers,
          content: defaultRequest.content,
          username: defaultRequest.username,
          password: defaultRequest.password,
          withCredentials: defaultRequest.withCredentials,
          timeout: defaultRequest.timeout,
          url: u,
          responseFormat: rf
        });
      };
    };
  };

  // output/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url2) {
      return url2 || "/";
    }
  };

  // output/Affjax.Web/index.js
  var get2 = /* @__PURE__ */ get(driver);

  // output/Data.Argonaut.Decode.Error/index.js
  var show3 = /* @__PURE__ */ show(showString);
  var show12 = /* @__PURE__ */ show(showInt);
  var TypeMismatch2 = /* @__PURE__ */ function() {
    function TypeMismatch3(value0) {
      this.value0 = value0;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return new TypeMismatch3(value0);
    };
    return TypeMismatch3;
  }();
  var UnexpectedValue = /* @__PURE__ */ function() {
    function UnexpectedValue2(value0) {
      this.value0 = value0;
    }
    ;
    UnexpectedValue2.create = function(value0) {
      return new UnexpectedValue2(value0);
    };
    return UnexpectedValue2;
  }();
  var AtIndex = /* @__PURE__ */ function() {
    function AtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtIndex2.create = function(value0) {
      return function(value1) {
        return new AtIndex2(value0, value1);
      };
    };
    return AtIndex2;
  }();
  var AtKey = /* @__PURE__ */ function() {
    function AtKey2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtKey2.create = function(value0) {
      return function(value1) {
        return new AtKey2(value0, value1);
      };
    };
    return AtKey2;
  }();
  var Named = /* @__PURE__ */ function() {
    function Named2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Named2.create = function(value0) {
      return function(value1) {
        return new Named2(value0, value1);
      };
    };
    return Named2;
  }();
  var MissingValue = /* @__PURE__ */ function() {
    function MissingValue2() {
    }
    ;
    MissingValue2.value = new MissingValue2();
    return MissingValue2;
  }();
  var showJsonDecodeError = {
    show: function(v) {
      if (v instanceof TypeMismatch2) {
        return "(TypeMismatch " + (show3(v.value0) + ")");
      }
      ;
      if (v instanceof UnexpectedValue) {
        return "(UnexpectedValue " + (stringify(v.value0) + ")");
      }
      ;
      if (v instanceof AtIndex) {
        return "(AtIndex " + (show12(v.value0) + (" " + (show(showJsonDecodeError)(v.value1) + ")")));
      }
      ;
      if (v instanceof AtKey) {
        return "(AtKey " + (show3(v.value0) + (" " + (show(showJsonDecodeError)(v.value1) + ")")));
      }
      ;
      if (v instanceof Named) {
        return "(Named " + (show3(v.value0) + (" " + (show(showJsonDecodeError)(v.value1) + ")")));
      }
      ;
      if (v instanceof MissingValue) {
        return "MissingValue";
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Error (line 24, column 10 - line 30, column 35): " + [v.constructor.name]);
    }
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";

  // output/Data.Argonaut.Decode.Decoders/index.js
  var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindEither);
  var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeEither);
  var decodeString = /* @__PURE__ */ function() {
    return caseJsonString(new Left(new TypeMismatch2("String")))(Right.create);
  }();
  var decodeJArray = /* @__PURE__ */ function() {
    var $52 = note(new TypeMismatch2("Array"));
    return function($53) {
      return $52(toArray($53));
    };
  }();
  var decodeBoolean = /* @__PURE__ */ function() {
    return caseJsonBoolean(new Left(new TypeMismatch2("Boolean")))(Right.create);
  }();
  var decodeArray = function(decoder) {
    return composeKleisliFlipped3(function() {
      var $89 = lmap2(Named.create("Array"));
      var $90 = traverseWithIndex2(function(i) {
        var $92 = lmap2(AtIndex.create(i));
        return function($93) {
          return $92(decoder($93));
        };
      });
      return function($91) {
        return $89($90($91));
      };
    }())(decodeJArray);
  };

  // output/Record/index.js
  var insert3 = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(a) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(a)(r);
            };
          };
        };
      };
    };
  };

  // output/Data.Argonaut.Decode.Class/index.js
  var bind2 = /* @__PURE__ */ bind(bindEither);
  var lmap3 = /* @__PURE__ */ lmap(bifunctorEither);
  var map7 = /* @__PURE__ */ map(functorMaybe);
  var gDecodeJsonNil = {
    gDecodeJson: function(v) {
      return function(v1) {
        return new Right({});
      };
    }
  };
  var gDecodeJson = function(dict) {
    return dict.gDecodeJson;
  };
  var decodeRecord = function(dictGDecodeJson) {
    var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
    return function() {
      return {
        decodeJson: function(json2) {
          var v = toObject(json2);
          if (v instanceof Just) {
            return gDecodeJson1(v.value0)($$Proxy.value);
          }
          ;
          if (v instanceof Nothing) {
            return new Left(new TypeMismatch2("Object"));
          }
          ;
          throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 103, column 5 - line 105, column 46): " + [v.constructor.name]);
        }
      };
    };
  };
  var decodeJsonString = {
    decodeJson: decodeString
  };
  var decodeJsonField = function(dict) {
    return dict.decodeJsonField;
  };
  var gDecodeJsonCons = function(dictDecodeJsonField) {
    var decodeJsonField1 = decodeJsonField(dictDecodeJsonField);
    return function(dictGDecodeJson) {
      var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var insert5 = insert3(dictIsSymbol)()();
        return function() {
          return function() {
            return {
              gDecodeJson: function(object) {
                return function(v) {
                  var fieldName = reflectSymbol2($$Proxy.value);
                  var fieldValue = lookup(fieldName)(object);
                  var v1 = decodeJsonField1(fieldValue);
                  if (v1 instanceof Just) {
                    return bind2(lmap3(AtKey.create(fieldName))(v1.value0))(function(val) {
                      return bind2(gDecodeJson1(object)($$Proxy.value))(function(rest) {
                        return new Right(insert5($$Proxy.value)(val)(rest));
                      });
                    });
                  }
                  ;
                  if (v1 instanceof Nothing) {
                    return new Left(new AtKey(fieldName, MissingValue.value));
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 127, column 5 - line 134, column 44): " + [v1.constructor.name]);
                };
              }
            };
          };
        };
      };
    };
  };
  var decodeJsonBoolean = {
    decodeJson: decodeBoolean
  };
  var decodeJson = function(dict) {
    return dict.decodeJson;
  };
  var decodeFieldId = function(dictDecodeJson) {
    var decodeJson1 = decodeJson(dictDecodeJson);
    return {
      decodeJsonField: function(j) {
        return map7(decodeJson1)(j);
      }
    };
  };
  var decodeArray2 = function(dictDecodeJson) {
    return {
      decodeJson: decodeArray(decodeJson(dictDecodeJson))
    };
  };

  // output/FRP.Requester/index.js
  var create2 = /* @__PURE__ */ create(monadSTEffect)(monadSTEffect);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var subscribeWithLog2 = /* @__PURE__ */ subscribeWithLog(showUnit);
  var show4 = /* @__PURE__ */ show(showJsonDecodeError);
  var showEither2 = /* @__PURE__ */ showEither(showString);
  var createColdWarmRequester = function(dictShow) {
    var pushWithLog2 = pushWithLog(showEither2(dictShow));
    return function(dictDecodeJson) {
      var decodeJson2 = decodeJson(dictDecodeJson);
      return function(route) {
        return function(notifyEvent) {
          return function __do3() {
            var v = create2();
            var v1 = create2();
            subscribeWithLog2(v.event)("[" + (route + "] request"))(function(v2) {
              return launchAff_(bind1(get2(json)(route))(function(res) {
                var resp = function() {
                  if (res instanceof Left) {
                    return new Left("Request error: " + printError(res.value0));
                  }
                  ;
                  if (res instanceof Right) {
                    var v3 = decodeJson2(res.value0.body);
                    if (v3 instanceof Left) {
                      return new Left("decode error: " + show4(v3.value0));
                    }
                    ;
                    if (v3 instanceof Right) {
                      return new Right(v3.value0);
                    }
                    ;
                    throw new Error("Failed pattern match at FRP.Requester (line 63, column 15 - line 65, column 35): " + [v3.constructor.name]);
                  }
                  ;
                  throw new Error("Failed pattern match at FRP.Requester (line 60, column 11 - line 65, column 35): " + [res.constructor.name]);
                }();
                return liftEffect2(pushWithLog2(v1.push)("[" + (route + "] response="))(resp));
              }));
            })();
            subscribe(notifyEvent)(function(v2) {
              return v.push(unit);
            })();
            return {
              requestPush: v.push,
              responseEvent: v1.event
            };
          };
        };
      };
    };
  };

  // output/Data.Argonaut.Aeson.Helpers/index.js
  var Mode = /* @__PURE__ */ function() {
    function Mode2(value0) {
      this.value0 = value0;
    }
    ;
    Mode2.create = function(value0) {
      return new Mode2(value0);
    };
    return Mode2;
  }();
  var isSingleConstructor_Constructor = {
    isSingleConstructor: function(v) {
      return true;
    }
  };
  var isSingleConstructor = function(dict) {
    return dict.isSingleConstructor;
  };
  var areAllConstructorsNullary_Argument = {
    areAllConstructorsNullary: function(v) {
      return false;
    }
  };
  var areAllConstructorsNullary = function(dict) {
    return dict.areAllConstructorsNullary;
  };
  var areAllConstructorsNullary_Constructor = function(dictAreAllConstructorsNullary) {
    var areAllConstructorsNullary1 = areAllConstructorsNullary(dictAreAllConstructorsNullary);
    return {
      areAllConstructorsNullary: function(v) {
        return areAllConstructorsNullary1($$Proxy.value);
      }
    };
  };

  // output/Data.Argonaut.Decode.Generic/index.js
  var bind3 = /* @__PURE__ */ bind(bindEither);
  var map8 = /* @__PURE__ */ map(functorEither);
  var decodeRepArgsArgument = function(dictDecodeJson) {
    var decodeJson2 = decodeJson(dictDecodeJson);
    return {
      decodeRepArgs: function(js) {
        return bind3(note(new TypeMismatch2("NonEmptyArray"))(uncons(js)))(function(v) {
          return map8(function($104) {
            return function(v1) {
              return {
                init: v1,
                rest: v.tail
              };
            }(Argument($104));
          })(decodeJson2(v.head));
        });
      }
    };
  };
  var decodeRepArgs = function(dict) {
    return dict.decodeRepArgs;
  };

  // output/Data.Argonaut.Aeson.Decode.Generic/index.js
  var map9 = /* @__PURE__ */ map(functorEither);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindEither);
  var show5 = /* @__PURE__ */ show(showString);
  var lmap4 = /* @__PURE__ */ lmap(bifunctorEither);
  var bind4 = /* @__PURE__ */ bind(bindEither);
  var pure4 = /* @__PURE__ */ pure(applicativeEither);
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindEither);
  var toJsonArray = /* @__PURE__ */ caseJson(/* @__PURE__ */ $$const(/* @__PURE__ */ singleton2(jsonNull)))(function($134) {
    return singleton2(id($134));
  })(function($135) {
    return singleton2(id($135));
  })(function($136) {
    return singleton2(id($136));
  })(function($137) {
    return singleton2(id($137));
  })(function($138) {
    return singleton2(id($138));
  });
  var decodingErr = function(name16) {
    return function(msg) {
      return new Named("When decoding a " + name16, msg);
    };
  };
  var decodeAeson$prime = function(dict) {
    return dict["decodeAeson'"];
  };
  var decodeAesonConstructor = function(dictDecodeRepArgs) {
    return function(dictIsSymbol) {
      return function(dictAreAllConstructorsNullary) {
        var areAllConstructorsNullary2 = areAllConstructorsNullary(dictAreAllConstructorsNullary);
        return function(dictIsSingleConstructor) {
          var isSingleConstructor2 = isSingleConstructor(dictIsSingleConstructor);
          return function(dictDecodeAeson$prime) {
            var decodeAeson$prime1 = decodeAeson$prime(dictDecodeAeson$prime);
            return {
              decodeAeson: function(o) {
                return function(thing) {
                  var mode = new Mode({
                    "_Mode_ConstructorIsSingle": isSingleConstructor2($$Proxy.value),
                    "_Mode_ConstructorsAreAllNullary": areAllConstructorsNullary2($$Proxy.value)
                  });
                  return decodeAeson$prime1(mode)(o)(thing);
                };
              }
            };
          };
        };
      };
    };
  };
  var decodeAeson = function(dict) {
    return dict.decodeAeson;
  };
  var genericDecodeAeson = function(dictGeneric) {
    var to2 = to(dictGeneric);
    return function(dictDecodeAeson) {
      var decodeAeson1 = decodeAeson(dictDecodeAeson);
      return function(o) {
        var $139 = map9(to2);
        var $140 = decodeAeson1(o);
        return function($141) {
          return $139($140($141));
        };
      };
    };
  };
  var checkTag = function(tagFieldName) {
    return function(expectedTag) {
      return composeKleisli2(function() {
        var $142 = note(new Named(show5(tagFieldName) + " property is missing", MissingValue.value));
        var $143 = lookup(tagFieldName);
        return function($144) {
          return $142($143($144));
        };
      }())(composeKleisli2(function() {
        var $145 = note(new TypeMismatch2(show5(tagFieldName) + " property is not a string"));
        return function($146) {
          return $145(toString($146));
        };
      }())(function(actualTag) {
        var $75 = actualTag !== expectedTag;
        if ($75) {
          return new Left(new Named("'tag' property has an incorrect value", new TypeMismatch2(actualTag)));
        }
        ;
        return new Right(unit);
      }));
    };
  };
  var decodeGeneralCase = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictDecodeRepArgs) {
      var decodeRepArgs2 = decodeRepArgs(dictDecodeRepArgs);
      return function(mode) {
        return function(options2) {
          return function(json2) {
            var name16 = reflectSymbol2($$Proxy.value);
            var $93 = {
              mode,
              options: options2
            };
            return bind4(note(new TypeMismatch2("expected an object"))(toObject(json2)))(function(objectJson) {
              return discard2(checkTag($93.options.sumEncoding.value0.tagFieldName)(name16)(objectJson))(function() {
                return bind4(function() {
                  var v = lookup($93.options.sumEncoding.value0.contentsFieldName)(objectJson);
                  if (v instanceof Just) {
                    return decodeRepArgs2(toJsonArray(v.value0));
                  }
                  ;
                  if (v instanceof Nothing) {
                    return decodeRepArgs2(singleton2(id($$delete3($93.options.sumEncoding.value0.tagFieldName)(objectJson))));
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Argonaut.Aeson.Decode.Generic (line 166, column 27 - line 170, column 115): " + [v.constructor.name]);
                }())(function(v) {
                  return pure4(v.init);
                });
              });
            });
          };
        };
      };
    };
  };
  var decodeAesonConstructor$prime = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    var decodeGeneralCase1 = decodeGeneralCase(dictIsSymbol);
    return function(dictDecodeRepArgs) {
      var decodeRepArgs2 = decodeRepArgs(dictDecodeRepArgs);
      var decodeGeneralCase2 = decodeGeneralCase1(dictDecodeRepArgs);
      return {
        "decodeAeson'": function(mode) {
          return function(options2) {
            return function(json2) {
              var name16 = reflectSymbol2($$Proxy.value);
              return lmap4(decodingErr(name16))(function() {
                var $102 = {
                  mode,
                  options: options2
                };
                if ($102["mode"]["value0"]["_Mode_ConstructorsAreAllNullary"]) {
                  return unsafeCrashWith("Unreachable: cannot have all nullary constructors and an `Argument` constructor at once.");
                }
                ;
                if ($102["mode"]["value0"]["_Mode_ConstructorIsSingle"] && !$102.options.tagSingleConstructors) {
                  return bind4(decodeRepArgs2(caseJsonArray(singleton2(json2))(function($147) {
                    return singleton2(id($147));
                  })(json2)))(function(v) {
                    return pure4(v.init);
                  });
                }
                ;
                return decodeGeneralCase2(mode)(options2)(json2);
              }());
            };
          };
        }
      };
    };
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate3 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsArgument = function(dictShow) {
    var show7 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show7(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate3(" ")(append2([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShow = function(dictGeneric) {
    var from3 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x) {
        return genericShow$prime1(from3(x));
      };
    };
  };

  // output/Data.Argonaut.Aeson.Options/index.js
  var TaggedObject = /* @__PURE__ */ function() {
    function TaggedObject2(value0) {
      this.value0 = value0;
    }
    ;
    TaggedObject2.create = function(value0) {
      return new TaggedObject2(value0);
    };
    return TaggedObject2;
  }();
  var defaultOptions = /* @__PURE__ */ function() {
    return {
      sumEncoding: new TaggedObject({
        tagFieldName: "tag",
        contentsFieldName: "contents"
      }),
      tagSingleConstructors: false,
      allNullaryToStringTag: true
    };
  }();

  // output/Proto.Symbols/index.js
  var showRecord2 = /* @__PURE__ */ showRecord()();
  var codeIsSymbol = {
    reflectSymbol: function() {
      return "code";
    }
  };
  var nameIsSymbol = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var symbolTypeIsSymbol = {
    reflectSymbol: function() {
      return "symbolType";
    }
  };
  var SymbolInfoIsSymbol = {
    reflectSymbol: function() {
      return "SymbolInfo";
    }
  };
  var completeIsSymbol = {
    reflectSymbol: function() {
      return "complete";
    }
  };
  var symbolsIsSymbol = {
    reflectSymbol: function() {
      return "symbols";
    }
  };
  var APISymbolsResponseIsSymbol = {
    reflectSymbol: function() {
      return "APISymbolsResponse";
    }
  };
  var areAllConstructorsNullary_Constructor2 = /* @__PURE__ */ areAllConstructorsNullary_Constructor(areAllConstructorsNullary_Argument);
  var gDecodeJsonCons2 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonString));
  var decodeRepArgsArgument2 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(gDecodeJsonNil)(symbolTypeIsSymbol)()())(nameIsSymbol)()())(codeIsSymbol)()())());
  var genericSymbolInfo = {
    to: function(x) {
      return x;
    },
    from: function(x) {
      return x;
    }
  };
  var showSymbolInfo = {
    show: /* @__PURE__ */ genericShow(genericSymbolInfo)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons(codeIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(nameIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(symbolTypeIsSymbol)(showString))(showString))(showString))))(SymbolInfoIsSymbol))
  };
  var genericAPISymbolsResponse = {
    to: function(x) {
      return x;
    },
    from: function(x) {
      return x;
    }
  };
  var showAPISymbolsResponse = {
    show: /* @__PURE__ */ genericShow(genericAPISymbolsResponse)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons(completeIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(symbolsIsSymbol)(/* @__PURE__ */ showArray(showSymbolInfo)))(showBoolean))))(APISymbolsResponseIsSymbol))
  };
  var decodeJsonSymbolInfo = {
    decodeJson: /* @__PURE__ */ genericDecodeAeson(genericSymbolInfo)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument2)(SymbolInfoIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(SymbolInfoIsSymbol)(decodeRepArgsArgument2)))(defaultOptions)
  };
  var decodeRepArgsArgument1 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonBoolean))(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(/* @__PURE__ */ decodeArray2(decodeJsonSymbolInfo)))(gDecodeJsonNil)(symbolsIsSymbol)()())(completeIsSymbol)()())());
  var decodeJsonAPISymbolsResponse = {
    decodeJson: /* @__PURE__ */ genericDecodeAeson(genericAPISymbolsResponse)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument1)(APISymbolsResponseIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(APISymbolsResponseIsSymbol)(decodeRepArgsArgument1)))(defaultOptions)
  };

  // output/Web.DOM.Document/foreign.js
  var getEffProp = function(name16) {
    return function(doc) {
      return function() {
        return doc[name16];
      };
    };
  };
  var url = getEffProp("URL");
  var documentURI = getEffProp("documentURI");
  var origin = getEffProp("origin");
  var compatMode = getEffProp("compatMode");
  var characterSet = getEffProp("characterSet");
  var contentType = getEffProp("contentType");
  var _documentElement = getEffProp("documentElement");
  function createElement(localName2) {
    return function(doc) {
      return function() {
        return doc.createElement(localName2);
      };
    };
  }

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name16, value13) {
    if (typeof window !== "undefined") {
      var ty = window[name16];
      if (ty != null && value13 instanceof ty) {
        return just(value13);
      }
    }
    var obj = value13;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name16) {
        return just(value13);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name16) {
    return function(value13) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name16, value13);
    };
  };

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name16) {
    return function(doctype) {
      return doctype[name16];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function setAttribute(name16) {
    return function(value13) {
      return function(element) {
        return function() {
          element.setAttribute(name16, value13);
        };
      };
    };
  }
  function _getAttribute(name16) {
    return function(element) {
      return function() {
        return element.getAttribute(name16);
      };
    };
  }

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp2 = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var children = getEffProp2("children");
  var _firstElementChild = getEffProp2("firstElementChild");
  var _lastElementChild = getEffProp2("lastElementChild");
  var childElementCount = getEffProp2("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }
  function querySelectorAll(selector) {
    return function(node) {
      return function() {
        return node.querySelectorAll(selector);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var map10 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map10(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.DOM.Element/index.js
  var map11 = /* @__PURE__ */ map(functorEffect);
  var toNode = unsafeCoerce;
  var getAttribute = function(attr) {
    var $6 = map11(toMaybe);
    var $7 = _getAttribute(attr);
    return function($8) {
      return $6($7($8));
    };
  };
  var fromNode = /* @__PURE__ */ unsafeReadProtoTagged("Element");

  // output/Web.DOM.Node/foreign.js
  var getEffProp3 = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var baseURI = getEffProp3("baseURI");
  var _ownerDocument = getEffProp3("ownerDocument");
  var _parentNode = getEffProp3("parentNode");
  var _parentElement = getEffProp3("parentElement");
  var childNodes = getEffProp3("childNodes");
  var _firstChild = getEffProp3("firstChild");
  var _lastChild = getEffProp3("lastChild");
  var _previousSibling = getEffProp3("previousSibling");
  var _nextSibling = getEffProp3("nextSibling");
  var _nodeValue = getEffProp3("nodeValue");
  var textContent = getEffProp3("textContent");
  function setTextContent(value13) {
    return function(node) {
      return function() {
        node.textContent = value13;
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }

  // output/Web.DOM.NodeList/foreign.js
  function toArray4(list) {
    return function() {
      return [].slice.call(list);
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var toParentNode = unsafeCoerce;
  var toEventTarget = unsafeCoerce;
  var toDocument = unsafeCoerce;

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Data/index.js
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var bind5 = /* @__PURE__ */ bind(bindEffect);
  var pure5 = /* @__PURE__ */ pure(applicativeEffect);
  var show6 = /* @__PURE__ */ show(showInt);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var createColdWarmRequester2 = /* @__PURE__ */ createColdWarmRequester(showAPISymbolsResponse)(decodeJsonAPISymbolsResponse);
  var subscribeWithLog3 = /* @__PURE__ */ subscribeWithLog(/* @__PURE__ */ showEither(showString)(showAPISymbolsResponse));
  var subscribeWithLog1 = /* @__PURE__ */ subscribeWithLog(showUnit);
  var setLoading = /* @__PURE__ */ traverse_2(function(el) {
    return setTextContent("...")(toNode(el));
  });
  var renderSymbolList = function(htmlDoc) {
    return function(symbols) {
      var appendOption = function(doc) {
        return function(listEl) {
          return function(v) {
            return function __do3() {
              var opt = createElement("option")(doc)();
              setAttribute("value")(v.symbolType + ("|" + v.code))(opt)();
              setTextContent(v.name)(toNode(opt))();
              return appendChild(toNode(opt))(toNode(listEl))();
            };
          };
        };
      };
      return function __do3() {
        var mList = querySelector("#symbol-list")(toParentNode(htmlDoc))();
        if (mList instanceof Nothing) {
          return unit;
        }
        ;
        if (mList instanceof Just) {
          setTextContent("")(toNode(mList.value0))();
          var doc$prime = toDocument(htmlDoc);
          return traverse_2(appendOption(doc$prime)(mList.value0))(symbols)();
        }
        ;
        throw new Error("Failed pattern match at Data (line 39, column 3 - line 44, column 51): " + [mList.constructor.name]);
      };
    };
  };
  var renderSymbolCounts = function(symbols) {
    return traverse_2(function(el) {
      return function __do3() {
        var mType = getAttribute("data-symbol-type")(el)();
        if (mType instanceof Nothing) {
          return unit;
        }
        ;
        if (mType instanceof Just) {
          var val = length(filter(function(v) {
            return v.symbolType === mType.value0;
          })(symbols));
          return setTextContent(mType.value0 + (": " + show6(val)))(toNode(el))();
        }
        ;
        throw new Error("Failed pattern match at Data (line 30, column 5 - line 34, column 60): " + [mType.constructor.name]);
      };
    });
  };
  var combineDataFRP = function(initEvent) {
    return function(notifyEvent) {
      return function __do3() {
        var win = windowImpl();
        var doc = document(win)();
        var nodes = bind5(querySelectorAll("[data-frp-symbol-count]")(toParentNode(doc)))(toArray4)();
        var els = mapMaybe(fromNode)(nodes);
        return when2(!$$null(els))(function __do4() {
          var v = createColdWarmRequester2("/APISymbols")(notifyEvent)();
          subscribeWithLog3(v.responseEvent)("[APISymbolsResponse](Data)")(function(resp) {
            if (resp instanceof Left) {
              return pure5(unit);
            }
            ;
            if (resp instanceof Right) {
              return function __do5() {
                renderSymbolCounts(resp.value0.symbols)(els)();
                return renderSymbolList(doc)(resp.value0.symbols)();
              };
            }
            ;
            throw new Error("Failed pattern match at Data (line 66, column 7 - line 70, column 41): " + [resp.constructor.name]);
          })();
          subscribeWithLog1(initEvent)("[init](Data)")(function(v1) {
            return function __do5() {
              setLoading(els)();
              return v.requestPush(unit)();
            };
          })();
          return unit;
        })();
      };
    };
  };

  // output/FFI.SSE/foreign.js
  var attachEventSource = (route) => (handler) => () => {
    const es = new EventSource(route);
    es.onmessage = () => {
      handler(void 0)();
    };
    es.onerror = () => {
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target5) {
          return function() {
            return target5.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";

  // output/Main/index.js
  var create5 = /* @__PURE__ */ create(monadSTEffect)(monadSTEffect);
  var pure6 = /* @__PURE__ */ pure(applicativeEffect);
  var subscribeWithLog4 = /* @__PURE__ */ subscribeWithLog(showUnit);
  var bindNotifyEvent = function __do() {
    var v = create5();
    attachEventSource("/sse/notify")(function(v1) {
      return v.push(unit);
    })();
    return v.event;
  };
  var bindDomEvent = function(doc) {
    return function(domPush) {
      return function __do3() {
        var onInit = eventListener(function(v) {
          return pushWithLogUnit(domPush)("[dom](Main)");
        })();
        addEventListener(domcontentloaded)(onInit)(false)(toEventTarget(doc))();
        return unit;
      };
    };
  };
  var main = function __do2() {
    var v = create5();
    var v1 = create5();
    subscribeWithLog4(v.event)("[dom](Main)")(function(v2) {
      return function __do3() {
        var notifyEvent = bindNotifyEvent();
        combineDataFRP(v1.event)(notifyEvent)();
        pushWithLogUnit(v1.push)("[begin](Main)")();
        return unit;
      };
    })();
    subscribeWithLog4(v1.event)("[begin](Main)")(function(v2) {
      return pure6(unit);
    })();
    var win = windowImpl();
    var doc = document(win)();
    bindDomEvent(doc)(v.push)();
    return unit;
  };

  // <stdin>
  main();
})();
