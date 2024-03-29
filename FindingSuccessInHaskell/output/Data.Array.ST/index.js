// Generated by purs version 0.12.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var unshift = function (a) {
    return $foreign.unshiftAll([ a ]);
};
var unsafeThaw = function ($11) {
    return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)($11);
};
var unsafeFreeze = function ($12) {
    return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)($12);
};
var thaw = $foreign.copyImpl;
var withArray = function (f) {
    return function (xs) {
        return function __do() {
            var v = thaw(xs)();
            var v1 = f(v)();
            return unsafeFreeze(v)();
        };
    };
};
var sortBy = function (comp) {
    var comp$prime = function (x) {
        return function (y) {
            var v = comp(x)(y);
            if (v instanceof Data_Ordering.GT) {
                return 1;
            };
            if (v instanceof Data_Ordering.EQ) {
                return 0;
            };
            if (v instanceof Data_Ordering.LT) {
                return -1 | 0;
            };
            throw new Error("Failed pattern match at Data.Array.ST (line 109, column 15 - line 114, column 1): " + [ v.constructor.name ]);
        };
    };
    return $foreign.sortByImpl(comp$prime);
};
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};
var sort = function (dictOrd) {
    return sortBy(Data_Ord.compare(dictOrd));
};
var shift = $foreign.shiftImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var run = function (st) {
    return Control_Bind.bind(Control_Monad_ST_Internal.bindST)(st)(unsafeFreeze)();
};
var push = function (a) {
    return $foreign.pushAll([ a ]);
};
var pop = $foreign.popImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var peek = $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var modify = function (i) {
    return function (f) {
        return function (xs) {
            return function __do() {
                var v = peek(i)(xs)();
                if (v instanceof Data_Maybe.Just) {
                    return $foreign.poke(i)(f(v.value0))(xs)();
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return false;
                };
                throw new Error("Failed pattern match at Data.Array.ST (line 195, column 3 - line 197, column 26): " + [ v.constructor.name ]);
            };
        };
    };
};
var freeze = $foreign.copyImpl;
module.exports = {
    run: run,
    withArray: withArray,
    peek: peek,
    modify: modify,
    pop: pop,
    push: push,
    shift: shift,
    unshift: unshift,
    sort: sort,
    sortBy: sortBy,
    sortWith: sortWith,
    freeze: freeze,
    thaw: thaw,
    unsafeFreeze: unsafeFreeze,
    unsafeThaw: unsafeThaw,
    empty: $foreign.empty,
    poke: $foreign.poke,
    pushAll: $foreign.pushAll,
    unshiftAll: $foreign.unshiftAll,
    splice: $foreign.splice,
    toAssocArray: $foreign.toAssocArray
};
