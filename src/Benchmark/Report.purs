module Benchmark.Report where

import Prelude

import Benchmark (BenchRes)
import Control.Monad.Eff (Eff)
import Data.Traversable (sequence_)
import Global (encodeURIComponent)
import OpenURL (OpenURLEff, openURL)
import Simple.JSON (writeJSON)

type ReportEff e = OpenURLEff e

report1 :: forall e. BenchRes -> Eff (ReportEff e) Unit
report1 br = openURL $ "http://jacereda.github.io/report/?" <> (encodeURIComponent $ writeJSON br)

report :: forall e. Array BenchRes -> Eff (ReportEff e) Unit
report brs = sequence_ $ report1 <$> brs
