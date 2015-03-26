module Test.Main where

import Debug.Trace
import Control.Monad.Eff
import Benchmark.Types
import Benchmark.Simple
import Data.Array(range, map)
import Data.Traversable(sequence)
import Debug.Foreign

type Variant = forall e. [Eff (|e) Number] -> CB

foreign import asequence
"""
function asequence(m) {
return function(a) {
  return function() {
    var n = a.length;
    var res = new Array(n);
    for (var i = 0; i < n; i++)
      res[i] = a[i](m);
    return res;
  };
};
}
""" :: forall a m. (Applicative m) => [m a] -> m [a]  

main :: Eff (trace :: Trace) Unit
main = go s
  where s :: Suite
        s = suite "suite1" $ asequencebms ++ sequencebms
        vals :: [Number]
        vals = [1, 10, 100, 1000, 10000]
        sequencebms :: [Benchmark]
        sequencebms = (bm "sequence" tsequence <$> vals)
        asequencebms :: [Benchmark]
        asequencebms =  (bm "asequence" tasequence <$> vals)
        bm :: String -> Variant -> Number -> Benchmark
        bm prefix variant n = benchmark name cb
          where cb :: CB
                cb = variant (map pass (range 1 n))
                name :: String
                name = prefix ++ show n
        pass :: Number -> Eff () Number
        pass x = do
          return x
        tsequence :: Variant
        tsequence s _ = do
          let r = sequence s
          return unit
        tasequence :: Variant
        tasequence s _ = do
          let r = asequence s
          return unit



