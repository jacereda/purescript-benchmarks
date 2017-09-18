"use strict";

exports.measure = function(f) {
  return function(input) {
    return function(times) {
      return function() {
	for (var i = 0, l = times, v = input; i < l; ++i)
	  f(v);
      };
    };
  };
};
