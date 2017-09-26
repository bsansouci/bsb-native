'use strict';
goog.module("bs-platform.js_null_undefined");


function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function iter(x, f) {
  if (x == null) {
    return /* () */0;
  } else {
    return f(x);
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  } else {
    return undefined;
  }
}

exports.bind     = bind;
exports.iter     = iter;
exports.from_opt = from_opt;
/* No side effect */
