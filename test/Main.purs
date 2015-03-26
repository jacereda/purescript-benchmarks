module Test.Main where

import Debug.Trace
import Control.Monad.Eff
import Benchmark.Types
import Benchmark.Simple
import Control.Monad
import Data.Array
import Data.Traversable(sequence)
import Debug.Foreign

pass :: forall e. Number -> Eff () Number
pass x = do
  return x

old :: forall e. [Eff (|e) Number] -> CB
old s _ = do
  let r = sequence s
  return unit

new :: forall e. [Eff (|e) Number] -> CB
new s _ = do
  let r = asequence s
  return unit

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

main :: Eff (trace :: Trace, suite :: SuiteEff) Unit
main = return (suite "suite1")
       >>= add "old1" old1 
       >>= add "new1" new1
       >>= add "old10" old10
       >>= add "new10" new10
       >>= add "old100" old100
       >>= add "new100" new100
       >>= add "old1000" old1000
       >>= add "new1000" new1000
       >>= add "old10000" old10000
       >>= add "new10000" new10000
       >>= go
  where a1 = range 0 0
        a10 = range 0 9
        a100 = range 0 99
        a1000 = range 0 999
        a10000 = range 0 9999
        s1 = map pass a1
        s10 = map pass a10
        s100 = map pass a100
        s1000 = map pass a1000        
        s10000 = map pass a10000
        old1 = old s1
        new1 = new s1
        old10 = old s10
        new10 = new s10
        old100 = old s100
        new100 = new s100
        old1000 = old s1000
        new1000 = new s1000
        old10000 = old s10000
        new10000 = new s10000

