module Benchmark.Event where

import Benchmark.Types

foreign import target
"""
function target(e) {
  return e.target;
}""" :: Event -> Benchmark

foreign import currentTarget
"""
function currentTarget(e) {
  return e.currentTarget;
}""" :: Event -> Suite
