module Benchmark.Suite where

import Control.Monad.Eff
import Debug.Trace
import Benchmark.Types

foreign import suiteWithOptions
"""
function suiteWithOptions(oo) {
  var bm = require('benchmark');
  var o = Object.create(oo);
  o.onAbort = function(e) { return oo.onAbort(e)(); };
  o.onComplete = function(e) { return oo.onComplete(e)(); };
  o.onCycle = function(e) { return oo.onCycle(e)(); };
  o.onError = function(e) { return oo.onError(e)(); };
  o.onReset = function(e) { return oo.onReset(e)(); };
  o.onStart = function(e) { return oo.onStart(e)(); };
  var s = new bm.Suite(o);
  return s;
}
""" :: SuiteOptions -> Suite

foreign import push
"""
function push(b) {
  return function(s) {
    return function() {
      s.push(b);
      return s;
    };
  };
}
""" :: forall e e2. Benchmark -> Suite -> Eff (suite :: SuiteEff | e) Suite

foreign import run
"""
function run(s) {
  return function() {
    s.run();
  };
}
""" :: forall e. Suite -> Eff (suite :: SuiteEff | e) Unit

sproperties :: Suite -> SuiteProperties
sproperties (Suite s) = s


