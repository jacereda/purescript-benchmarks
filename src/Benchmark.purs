module Benchmark where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Rec.Loops (whileM)
import Data.Array (drop, length)
import Data.Foldable (maximum)
import Data.Int (toNumber, round)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple.Nested (get1, get2, get3)
import Math.Statistics (linreg)
import Math.Statistics.Types (XYSample)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (writeTextFile)
import System.Clock (CLOCK, nanoseconds)
import Text.Format (format, width, precision)

foreign import measure :: forall a b e. (a -> b) -> a -> Int -> Eff (clock :: CLOCK | e) Unit

type BenchEff e = Eff (console :: CONSOLE, clock :: CLOCK, ref :: REF | e)

type ReportEff e = BenchEff (fs :: FS, exception :: EXCEPTION | e)

type VarRes = { name :: String
              , a :: Number
              , b :: Number
              , r :: Number
              , xys :: XYSample
              }

type BenchRes = { name :: String
                , results :: Array VarRes
                }

variant' :: forall a b e. String -> (a -> b) -> a -> Number -> Eff (clock :: CLOCK, ref :: REF | e) VarRes
variant' name fn input limit = do
  let mfi = measure fn input
  t0 <- nanoseconds
  let under t = do
        t1 <- nanoseconds
        pure $ t1 - t0 < t
  cnt <- newRef 1.0
  rawxys <- whileM (under limit) do
    times <- readRef cnt
    let itimes = round times
        ftimes = toNumber itimes
        m = mfi itimes
    te <- do
        s <- nanoseconds
        m
        e <- nanoseconds
        pure $ e - s
    writeRef cnt $ max (ftimes + 1.0) (ftimes * 1.05)
    pure {x: ftimes, y: te}
  let xys = drop (length rawxys / 3) rawxys
  let reg = linreg xys
      fm f = fromMaybe 0.0 $ f <$> reg
      a = fm get1
      b = fm get2
      r = fm get3
  pure {name: name, a: a, b: b, r: r, xys:xys}


variant :: forall a b e. String -> (a -> b) -> a -> Number -> BenchEff e VarRes
variant name fn input limit = do
  res <- variant' name fn input limit
  let nshow = format (width 16 <> precision 3)
  log $ format (width 16) name <> " "
    <> nshow res.b <> " ns "
    <> nshow (res.r * res.r) <> " r2 "
    <> nshow (1000000000.0 / res.b) <> " ops/s "
  pure res

  
  
benchmarkFor :: forall e. Number -> String -> Array (Number -> BenchEff e VarRes) -> BenchEff e BenchRes
benchmarkFor limit name variants = do
  log $ "Running benchmark " <> name
  let nslimit = limit * 1000000000.0
  _ <- variant "warmup" id 0 nslimit
  results <- sequence $ (_ $ nslimit) <$> variants
  pure { name: name, results: results }

benchmark :: forall e. String -> Array (Number -> BenchEff e VarRes) -> BenchEff e BenchRes
benchmark = benchmarkFor 5.0

report :: forall e. BenchRes -> Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e) Unit
report r = do
  let out = r.name <> "-benchmark.html"
  writeTextFile UTF8 (r.name <> "-benchmark.html") (header <> overview <> charts <> footer)
  log $ "Report written to " <> out
  where header = """
<html>
  <head>
    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <script type="text/javascript">
      google.charts.load('current', {'packages':['corechart']});
"""
        footer = "</script></head><body>" <> divs <> "</body></html>"
        divs =  "<div id=\"overview\"></div>" <> (joinWith "\n" $ div <$> r.results)
        div v = "<div id=\"" <> v.name <> "\"></div>"
        overview = """
      google.charts.setOnLoadCallback(function() {
        var data = new google.visualization.arrayToDataTable([['Variant','Nanoseconds'],""" <> ts <> """]);
        var options = {title:'""" <> r.name <> """',
                       height:""" <> show (max 100 $ 30 * length r.results) <> """,
                       hAxis: {title: 'Nanoseconds', minValue:0},
                       vAxis: {title: 'Variant'},
                       legend:{position:'none'},
                       explorer:{},
                      };
        new google.visualization.BarChart(document.getElementById('overview')).draw(data, options);
      });
"""
        ts = joinWith "," $ tval <$> r.results
        tval vr = "['" <> vr.name <> "'," <> show (vr.b :: Number) <> "]"
        charts = joinWith "\n" $ chart <$> r.results
        chart v = """
      google.charts.setOnLoadCallback(function() {
        var data = new google.visualization.arrayToDataTable([['Iterations','Nanoseconds'],""" <> ssamples <> """]);
        var options = {title:'""" <> v.name <> """',
                       chartArea: {width:'55%'},
                       hAxis: {title: 'Iterations', minValue:0, maxValue:""" <> show maxx <> """},
                       vAxis: {title: 'Nanoseconds', minValue:0, maxValue:""" <> show maxy <> """},
                       trendlines: { 0: { type: 'linear', showR2: true, visibleInLegend: true } },
                       explorer:{},
                      };
        new google.visualization.ScatterChart(document.getElementById('""" <> v.name <> """')).draw(data, options);
      });
"""
          where maxx = fromMaybe 0.0 $ maximum $ (_.x) <$> v.xys
                maxy = fromMaybe 0.0 $ maximum $ (_.y) <$> v.xys
                ssamples = joinWith "," $ map (\{x:x,y:y} -> "[" <> show x <> "," <> show y <> "]") v.xys
