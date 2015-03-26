module Benchmark.Benchmark where

import Benchmark.Types

foreign import benchmarkWithOptions
"""
function benchmarkWithOptions(oo) {
  var bm = require('benchmark');
  var o = Object.create(oo);
  o.onAbort = oo.onAbort(o);
  o.onComplete = oo.onComplete(o);
  o.onCycle = oo.onCycle(o);
  o.onError = oo.onError(o);
  o.onReset = oo.onReset(o);
  o.onStart = oo.onStart(o);
  return new bm.Benchmark(o);
}
""" :: BenchmarkOptions -> Benchmark

foreign import summary
"""
function summary(b) {
  return String(b);
}
""" :: Benchmark -> String

foreign import stats
"""
function stats(b) {
  return b.stats;
}
""" :: Benchmark -> Stats

