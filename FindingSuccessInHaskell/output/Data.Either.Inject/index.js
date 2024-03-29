// Generated by purs version 0.12.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Inject = function (inj, prj) {
    this.inj = inj;
    this.prj = prj;
};
var prj = function (dict) {
    return dict.prj;
};
var injectReflexive = new Inject(Control_Category.identity(Control_Category.categoryFn), Data_Maybe.Just.create);
var injectLeft = new Inject(Data_Either.Left.create, Data_Either.either(Data_Maybe.Just.create)(Data_Function["const"](Data_Maybe.Nothing.value)));
var inj = function (dict) {
    return dict.inj;
};
var injectRight = function (dictInject) {
    return new Inject(function ($1) {
        return Data_Either.Right.create(inj(dictInject)($1));
    }, Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(prj(dictInject)));
};
module.exports = {
    inj: inj,
    prj: prj,
    Inject: Inject,
    injectReflexive: injectReflexive,
    injectLeft: injectLeft,
    injectRight: injectRight
};
