module Test.Main where

import Debug.Trace
import Control.Monad.Eff
import Benchmark.Types
import Benchmark.Simple
import Data.Maybe
import Data.Array(range, map)
import Data.Traversable(sequence)
import Data.Foldable(foldl)
import Debug.Foreign

type Sequence = forall a m. (Applicative m) => [m a] -> m [a]

foreign import apush """
function apush(a) {
  return function(x) {
    a.push(x);
    return a;
  };
}""" :: forall a. [a] -> a -> [a]

asequence :: forall a m. (Applicative m) => [m a] -> m [a]
asequence = foldl step (pure [])
  where step :: m [a] -> m a -> m [a]
        step sofar x = apush <$> sofar <*> x

main :: Eff (trace :: Trace) Unit
main = do
  go s
  where s :: Suite
        s = suite "suite1" ( (bm "asequence" asequence <$> vals)
                             ++ (bm "sequence" sequence <$> vals)                             
                           )
        vals = [1,10,100,1000]
        bm :: String -> Sequence -> Number -> Benchmark
        bm prefix seq n = benchmark (prefix ++ show n) \_ -> do
          s <- seq work
          return unit
          where work :: forall e. [Eff (|e) Unit]
                work = map (const (return unit)) (range 1 n)
