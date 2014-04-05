module Benchmark where

import Control.Monad
import Control.DeepSeq
import Criterion.Main

import Circle
import Position
import Intersections
import Test

getRandomCircles :: Int -> IO [Circle]
getRandomCircles n = do
  cs <- replicateM n getRandomCircle
  return cs
  
instance NFData Position
instance NFData Circle


benchmark = do
  let as = [1,2,3]
  let ns = [ c*10^e | e <- [0..1], c<- [1..9] ]
  let alg1 = solve 1
  let alg2 = solve 2
  let alg3 = solve 3
  cs <- mapM getRandomCircles ns
  -- yeah my code sucks but I tried to write macros first and it sucked balls because of the type chaos so now i'm doing this shit and it's much better
  defaultMain [
    bgroup "algorithm 1" [
       bench "10 circles" $ nf alg1 (cs !! 0)
       , bench "20 circles" $ nf alg1 (cs !! 1)
       , bench "30 circles" $ nf alg1 (cs !! 2)
       , bench "40 circles" $ nf alg1 (cs !! 3)
       , bench "50 circles" $ nf alg1 (cs !! 4)
       , bench "60 circles" $ nf alg1 (cs !! 5)
       , bench "70 circles" $ nf alg1 (cs !! 6)
       , bench "80 circles" $ nf alg1 (cs !! 7)
       , bench "90 circles" $ nf alg1 (cs !! 8)
       ]
    , bgroup "algorithm 2" [
       bench "10 circles" $ nf alg2 (cs !! 0)
       , bench "20 circles" $ nf alg2 (cs !! 1)
       , bench "30 circles" $ nf alg2 (cs !! 2)
       , bench "40 circles" $ nf alg2 (cs !! 3)
       , bench "50 circles" $ nf alg2 (cs !! 4)
       , bench "60 circles" $ nf alg2 (cs !! 5)
       , bench "70 circles" $ nf alg2 (cs !! 6)
       , bench "80 circles" $ nf alg2 (cs !! 7)
       , bench "90 circles" $ nf alg2 (cs !! 8)
       ]
    -- , bgroup "algorithm 3" [
    --    bench "10 circles" $ nf alg3 (cs !! 0)
    --    , bench "20 circles" $ nf alg3 (cs !! 1)
    --    , bench "30 circles" $ nf alg3 (cs !! 2)
    --    , bench "40 circles" $ nf alg3 (cs !! 3)
    --    , bench "50 circles" $ nf alg3 (cs !! 4)
    --    , bench "60 circles" $ nf alg3 (cs !! 5)
    --    , bench "70 circles" $ nf alg3 (cs !! 6)
    --    , bench "80 circles" $ nf alg3 (cs !! 7)
    --    , bench "90 circles" $ nf alg3 (cs !! 8)
    --    ]
    ]
