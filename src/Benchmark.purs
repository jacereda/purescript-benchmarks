module Benchmark where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Rec.Loops (whileM)
import Data.Array (drop, length)
import Data.Int (toNumber, round)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple.Nested (get1, get2, get3)
import Math.Statistics (linreg)
import Math.Statistics.Types (XYSample)
import System.Clock (CLOCK, nanoseconds)
import Text.Format (format, width, precision)


type Input a = { name :: String, val :: a }

type BenchEff e = (console :: CONSOLE, clock :: CLOCK, ref :: REF | e)

type ReportEff e = BenchEff (exception :: EXCEPTION | e)

type VarRes = { name :: String
              , a :: Number
              , b :: Number
              , r :: Number
              , xys :: XYSample
              }

type BenchRes = { name :: String
                , result :: Array VarRes
                }

foreign import measure :: forall b e. (Unit -> b) -> Int -> Eff (clock :: CLOCK | e) Unit

variant' :: forall b e. String -> (Unit -> b) -> Number -> Eff (clock :: CLOCK, ref :: REF | e) VarRes
variant' name fn limit = do
  let mfi = measure fn
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
  let xys = drop (length rawxys / 2) rawxys
  let reg = linreg xys
      fm f = fromMaybe 0.0 $ f <$> reg
      a = fm get1
      b = fm get2
      r = fm get3
  pure {name: name, a, b: b, r: r, xys:xys}

input :: forall a. String -> a -> Input a
input name val = { name: name, val: val }
  
summary :: forall e. VarRes -> Eff (console :: CONSOLE | e) VarRes
summary res = do
  let nshow = format (width 16 <> precision 3)
      sshow = format (width 5 <> precision 3)
  log $ format (width 16) res.name
    <> nshow res.b <> " ns "
    <> sshow (res.r * res.r) <> " r2 "
    <> nshow (1000000000.0 / res.b) <> " ops/s "
  pure res

variantFor :: forall a b e. Number -> String -> (a -> b) -> Input a -> Eff (BenchEff e) VarRes
variantFor limit name fn inp = summary =<< variant' (name <> "/" <> inp.name) (\_ -> fn inp.val) (limit * 1000000000.0)

variant :: forall a b e. String -> (a -> b) -> Input a -> Eff (BenchEff e) VarRes
variant = variantFor 5.0

benchmark :: forall e. String -> Array (Eff (BenchEff e) VarRes) -> Eff (BenchEff e) BenchRes
benchmark name vars = do
  log ("Running benchmark " <> name)
  result <- sequence vars
  pure {name: name, result: result}

