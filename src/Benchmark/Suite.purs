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
  o.benchmarks = undefined;
  var s = new bm.Suite(o);
  for (var b in oo.benchmarks)
    s.push(oo.benchmarks[b]);
  return s;
}
""" :: SuiteOptions -> Suite

foreign import run
"""
function run(s) {
  return function() {
    s.run();
  };
}
""" :: forall e. Suite -> Eff e Unit

properties :: Suite -> SuiteProperties
properties (Suite s) = s


