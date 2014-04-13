module Benchmark.Benchmark where

import Control.Monad
import System.IO
import System.Random
import Criterion.Main
import Geometry.Circle
import Geometry.Position
import Text.Printf
import Intersections.Intersections
import Control.DeepSeq

instance NFData Position
instance NFData Circle

-- Generate a random scaled circle
randomScaledCircle :: Double -> IO Circle
randomScaledCircle sc = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return $ Cir (Pos x y) (sc*r)

-- Generate n random circles with the radii scaled by sc
randomScaledCircles :: Int -> Double -> IO [Circle]
randomScaledCircles n sc = replicateM n $ randomScaledCircle sc

-- Benchmark Intersections :: Algorithmnr -> Nr of circles -> Scale of radii -> IO ()
benchIntersections :: Int -> Int -> Double -> IO ()
benchIntersections na nc sc = do
    cs <- randomScaledCircles nc sc
    defaultMain [benchC na nc sc cs]

benchC :: Int -> Int -> Double  -> [Circle] -> Benchmark
benchC na nc sc cs  = bench str $ nf (solve na) cs
    where str = show na ++ "_" ++ show nc ++ "_" ++ show sc

nas = [ 1..3 ]
ncs = [ c*10^e | e <- [0..1], c<- [1..9] ]
scs = [ 10**x | x <- [(-2)..2 ] ]

benchmark = benchIntersections 1 10 1
