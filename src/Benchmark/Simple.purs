module Benchmark.Simple where

import Control.Monad.Eff
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
                sprops = sproperties $ currentTarget e

suite :: String -> Suite
suite name = suiteWithOptions defSuiteOptions { name = name }

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
                       , fn : noopCB "fn"
                       }
  where noopBenchmarkCB :: BenchmarkCB
        noopBenchmarkCB b = do
          return true
        msgBenchmarkCB :: String -> BenchmarkCB
        msgBenchmarkCB msg b = do
          trace msg
          return true
          
        noopCB :: String -> CB
        noopCB msg _ = do
--          trace msg
          return unit

benchmark :: String -> CB -> Benchmark
benchmark name fn = benchmarkWithOptions defBenchmarkOptions { fn = fn
                                                        , name = name }

add :: forall e. String -> CB -> Suite -> Eff (suite :: SuiteEff |e) Suite
add name cb = push (benchmark name cb)

go = run
