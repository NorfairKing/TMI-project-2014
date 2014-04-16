module Benchmark.Benchmark where

import Control.Monad
import Control.DeepSeq
import System.IO
import System.Random
import Criterion.Main
import Criterion.Measurement
import Geometry.Circle
import Geometry.Position
import Text.Printf
import Intersections.Intersections
import Control.DeepSeq
import Data.Maybe
import Text.CSV

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

{-
-- Benchmark Intersections :: Algorithmnr -> Nr of circles -> Scale of radii -> IO ()
benchIntersections :: Int -> Int -> Double -> IO ()
benchIntersections na nc sc = do
    cs <- randomScaledCircles nc sc
    defaultMain [benchC na nc sc cs]

benchC :: Int -> Int -> Double  -> [Circle] -> Benchmark
benchC na nc sc cs  = bench str $ nf (solve na) cs
    where str = show na ++ "_" ++ show nc ++ "_" ++ show sc
-}

nas = [ 1..3 ]
ncs = [ c*10^e | e <- [0..1], c<- [1..9] ]
scs = [ 10**x | x <- [(-2)..2 ] ]

benchmark = putStrLn "hi"  -- benchIntersections 1 10 1

test :: [Circle] -> IO [Position]
test cs = return $ fromJust $  (solve 1) cs

ioSolve :: Int -> [Circle] -> IO [Position]
ioSolve na cs | na `elem` nas = return $!! (fromJust . solve na) cs
ioSolve _ _ = return []

--benchSolve :: Int -> Int -> Double -> IO Double
benchSolve na nc sc = do
    cs <- randomScaledCircles nc sc
    cs `deepseq` time_ $ ioSolve na cs


getCSVResults nt na nc sc = do
    ts <- replicateM nt $ benchSolve na nc sc
    return $ printCSV $ (map toStr) ts
    where toStr t = [show na, show nc, show sc, show t]

