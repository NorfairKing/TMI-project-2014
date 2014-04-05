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
       bench "algorithm 1, 10 cs" $ nf alg1 (cs !! 0)
       , bench "algorithm 1, 20 cs" $ nf alg1 (cs !! 1)
       , bench "algorithm 1, 30 cs" $ nf alg1 (cs !! 2)
       ]
    , bgroup "algorithm 2" [
       bench "algorithm 2, 10 cs" $ nf alg2 (cs !! 0)
       , bench "algorithm 2, 20 cs" $ nf alg2 (cs !! 1)
       , bench "algorithm 2, 30 cs" $ nf alg2 (cs !! 2)
       ]
    -- , bgroup "algorithm 3" [
    --    bench "algorithm 3, 10 cs" $ nf alg3 (cs !! 0)
    --    , bench "algorithm 3, 20 cs" $ nf alg3 (cs !! 1)
    --    , bench "algorithm 3, 30 cs" $ nf alg3 (cs !! 2)
    --    ]
    ]
