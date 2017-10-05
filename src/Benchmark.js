"use strict";

//require('v8').setFlagsFromString('--minimal');
//require('v8').setFlagsFromString('--trace-deopt');
exports.measure = function(f) {
  return function(times) {
    return function() {
      for (var i = 0, l = times; i < l; ++i)
	f();
    };
  };
};

