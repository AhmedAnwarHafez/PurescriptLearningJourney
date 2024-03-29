// Generated by purs version 0.12.5
"use strict";
var Data_Array = require("../Data.Array/index.js");
var Data_Char_Unicode = require("../Data.Char.Unicode/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Data_String_Common = require("../Data.String.Common/index.js");
var isWord = function (word) {
    var v = Data_String_Common["null"](word);
    if (v) {
        return Data_Maybe.Nothing.value;
    };
    if (!v) {
        var v1 = Data_Foldable.all(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Char_Unicode.isAlpha)(Data_String_CodeUnits.toCharArray(word));
        if (!v1) {
            return Data_Maybe.Nothing.value;
        };
        if (v1) {
            return new Data_Maybe.Just(word);
        };
        throw new Error("Failed pattern match at Ch2 (line 25, column 13 - line 27, column 34): " + [ v1.constructor.name ]);
    };
    throw new Error("Failed pattern match at Ch2 (line 22, column 5 - line 27, column 34): " + [ v.constructor.name ]);
};
var isAnagram = function (word1) {
    return function (word2) {
        return Data_Eq.eq(Data_Eq.eqArray(Data_Eq.eqChar))(Data_Array.sort(Data_Ord.ordChar)(Data_String_CodeUnits.toCharArray(word1)))(Data_Array.sort(Data_Ord.ordChar)(Data_String_CodeUnits.toCharArray(word2)));
    };
};
var checkAnagram = function (word1) {
    return function (word2) {
        var v = isWord(word1);
        if (v instanceof Data_Maybe.Nothing) {
            return "The first word is invalid";
        };
        if (v instanceof Data_Maybe.Just) {
            var v1 = isWord(word2);
            if (v1 instanceof Data_Maybe.Nothing) {
                return "The second word is invalid";
            };
            if (v1 instanceof Data_Maybe.Just) {
                var v2 = isAnagram(v.value0)(v1.value0);
                if (v2) {
                    return "It is Anagram";
                };
                if (!v2) {
                    return "Nope";
                };
                throw new Error("Failed pattern match at Ch2 (line 38, column 21 - line 40, column 34): " + [ v2.constructor.name ]);
            };
            throw new Error("Failed pattern match at Ch2 (line 35, column 13 - line 40, column 34): " + [ v1.constructor.name ]);
        };
        throw new Error("Failed pattern match at Ch2 (line 32, column 5 - line 40, column 34): " + [ v.constructor.name ]);
    };
};
module.exports = {
    isAnagram: isAnagram,
    isWord: isWord,
    checkAnagram: checkAnagram
};
