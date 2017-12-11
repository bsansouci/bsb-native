'use strict';


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

export {
  bind     ,
  iter     ,
  from_opt ,
  
}
/* No side effect */
