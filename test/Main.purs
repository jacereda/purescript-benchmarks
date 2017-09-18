module Test.Main where

import Prelude

import Benchmark (ReportEff, benchmark, variant, report)
import Data.Array (replicate)
import Data.Foldable (foldMap, foldr)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)

main :: forall e. ReportEff e Unit
main = do
  let a1000 = replicate 1000 1
      a2000 = replicate 2000 1
      a3000 = replicate 3000 1
      a4000 = replicate 4000 1
      a5000 = replicate 5000 1
      fr = foldr (+) 0
      fm = ala Additive foldMap
  report =<< benchmark "test" [ variant "foldr1000" fr a1000
                              , variant "foldr2000" fr a2000
                              , variant "foldr3000" fr a3000
                              , variant "foldr4000" fr a4000
                              , variant "foldr5000" fr a5000 
                              , variant "foldMap1000" fm a1000
                              , variant "foldMap2000" fm a2000
                              , variant "foldMap3000" fm a3000
                              , variant "foldMap4000" fm a4000
                              , variant "foldMap5000" fm a5000
                              ]
