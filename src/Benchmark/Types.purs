module Benchmark.Types where

import Control.Monad.Eff
import Debug.Trace

foreign import data Event :: *
foreign import data Benchmark :: *

type BenchmarkCB = forall e. Benchmark -> Eff (trace :: Trace |e) Boolean

type SuiteCB = Event -> Eff (trace :: Trace) Boolean

type CB = forall e. Number -> Eff (|e) Unit

type Stats =  { deviation :: Number
              , mean :: Number
              , moe :: Number
              , rme :: Number
              , sample :: [Number]
              , sem :: Number
              , variance :: Number
              }

type Platform = { description :: String
                , layout :: String
                , manufacturer :: String
                , name :: String
                , os :: String
                , prerelease :: String
                , product :: String
                , version :: String
                }

type BenchmarkOptions = { async :: Boolean
                        , defer :: Boolean
                        , delay :: Number
                        , initCount :: Number
                        , maxTime :: Number
                        , minSamples :: Number
                        , minTime :: Number
                        , name :: String
                        , onAbort :: BenchmarkCB
                        , onComplete :: BenchmarkCB
                        , onCycle :: BenchmarkCB
                        , onError :: BenchmarkCB
                        , onReset :: BenchmarkCB
                        , onStart :: BenchmarkCB
                        , fn :: CB
                        }

type SuiteOptions = { name :: String
                    , onAbort :: SuiteCB
                    , onComplete :: SuiteCB
                    , onCycle :: SuiteCB
                    , onError :: SuiteCB
                    , onReset :: SuiteCB
                    , onStart :: SuiteCB
                    , benchmarks :: [Benchmark]
                    }


type BenchmarkProperties = { version :: String
                           , aborted :: Boolean
                           , compiled :: Boolean
                           , count :: Number
                           , cycles :: Number
                           , running :: Boolean
                           , options :: BenchmarkOptions
                           , platform :: Platform
                           }

type SuiteProperties = { aborted :: Boolean
                       , length :: Number
                       , running :: Boolean
                       , name :: String
                       }

newtype Suite = Suite SuiteProperties
