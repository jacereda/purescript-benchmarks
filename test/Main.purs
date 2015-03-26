module Test.Main where

import Debug.Trace
import Control.Monad.Eff
import Benchmark.Types
import Benchmark.Simple
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

main :: Eff (trace :: Trace) Unit
main = go s
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
        ob1 :: Benchmark
        ob1 = benchmark "old1" old1 
        nb1 = benchmark "new1" new1
        ob10 = benchmark "old10" old10
        nb10 = benchmark "new10" new10
        ob100 = benchmark "old100" old100
        nb100 = benchmark "new100" new100
        ob1000 = benchmark "old1000" old1000
        nb1000 = benchmark "new1000" new1000
        ob10000 = benchmark "old10000" old10000
        nb10000 = benchmark "new10000" new10000
        s :: Suite
        s = suite "suite1" [ ob1
                           , nb1
                           , ob10
                           , nb10
                           , ob100
                           , nb100
                           , ob1000
                           , nb1000
                           , ob10000
                           , nb10000
                           ]

