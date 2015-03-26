module Benchmark.Platform where

import Benchmark.Types

foreign import platform "var platform = String((typeof(Benchmark) != 'undefined' && Benchmark || require('benchmark')).platform)" :: String
