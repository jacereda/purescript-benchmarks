module Benchmark.Simple where

import Benchmark.Types
import Benchmark.Suite
import Benchmark.Benchmark
import Benchmark.Event
import Benchmark.Platform
import Debug.Trace

defSuiteOptions :: SuiteOptions
defSuiteOptions = { name : "<Unnamed suite>"
                  , onStart : startCB
                  , onComplete : noopSuiteCB
                  , onCycle : cycleCB
                  , onAbort : msgSuiteCB "Suite aborted"
                  , onError : msgSuiteCB "Suite failed"
                  , onReset : msgSuiteCB "Suite reset"
                  , benchmarks : []
                  }
  where cycleCB :: SuiteCB
        cycleCB event = do
          trace $ summary $ target event
          return true
        noopSuiteCB :: SuiteCB
        noopSuiteCB e = do
          return true
        msgSuiteCB :: String -> SuiteCB
        msgSuiteCB msg e = do
          trace msg
          return true
        startCB :: SuiteCB
        startCB e = do
          trace $ "Running suite " ++ sname ++ " for " ++ platform
          return true
          where sname = sprops.name
                sprops = properties suite
                suite = currentTarget e

suite :: String -> [Benchmark] -> Suite
suite name benchmarks = suiteWithOptions defSuiteOptions { name = name
                                                         , benchmarks = benchmarks
                                                         }

defBenchmarkOptions :: BenchmarkOptions
defBenchmarkOptions =  { async : false
                       , defer : false
                       , delay : 0.005
                       , initCount : 1
                       , maxTime : 5
                       , minSamples : 5
                       , minTime : 0
                       , name : "<Unnamed>"
                       , onStart : noopBenchmarkCB
                       , onCycle : noopBenchmarkCB
                       , onComplete : noopBenchmarkCB
                       , onAbort : msgBenchmarkCB "Benchmark aborted"
                       , onError : msgBenchmarkCB "Benchmark failed"
                       , onReset : msgBenchmarkCB "Benchmark reset"
                       , fn : noopCB
                       }
  where noopBenchmarkCB :: BenchmarkCB
        noopBenchmarkCB b = return true
        msgBenchmarkCB :: String -> BenchmarkCB
        msgBenchmarkCB msg b = do
          trace msg
          return true
        noopCB :: CB
        noopCB _ = return unit

benchmark :: String -> CB -> Benchmark
benchmark name fn = benchmarkWithOptions defBenchmarkOptions { fn = fn
                                                             , name = name }

go = run
