module Test.Main where

import Prelude

import Benchmark (benchmark, variant, input, Input, BenchEff)
import Benchmark.Report (ReportEff, report)
import Control.Monad.Eff (Eff)
import Data.Array (foldl, range, replicate, zipWith)
import Data.Foldable (foldr, sum)
import Data.Traversable (sequence)

main :: forall e. Eff (ReportEff (BenchEff e)) Unit
main = do
  let frep :: Int -> Array Number
      frep = flip replicate 0.0
      irep :: Int -> Array Int
      irep = flip replicate 0
      f1000 :: Input (Array Number)
      f1000 = input "1000flt" $ frep 1000
      finputs = [  input "1000flt" $ frep 1000
                , input "2000flt" $ frep 2000
                ]
      iinputs = [ input "1000int" $ irep 1000
                , input "2000int" $ irep 2000
                ]
      ivariants = [ variant "sum" \x -> sum x
                  , variant "foldl" $ foldl (+) zero 
                  , variant "foldr" $ foldr (+) zero
                 ]
      fvariants = [ variant "sum" \x -> sum x
                  , variant "foldl" $ foldl (+) zero 
                  , variant "foldr" $ foldr (+) zero
                  ]
      inputs = [ input "1000" $ replicate 1000 one
               , input "2000" $ replicate 2000 one
               ]
      variants = [ variant "foldl" $ foldl (+) 0
                 , variant "foldr" $ foldr (+) 0
                 ]
      full = zipWith ($) variants inputs
  let id1000 :: forall a. Array (a -> a)
      id1000 = replicate 100 id
      fempty :: Input (Array Number)
      fempty = input "empty" []
  report =<< sequence [ benchmark "itest" (ivariants <*> iinputs)
                      , benchmark "ftest" (fvariants <*> finputs)
                      , benchmark "apply" [ variant "apply" (apply id1000) f1000 ]
                      , benchmark "append" [ variant "append" (\x -> append x x) f1000 ]
                      , benchmark "range" [ variant "range" (\_ -> range 0 1000) f1000 ]
                      ]
