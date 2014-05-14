module Benchmark.Case where

import Control.DeepSeq
import Control.Monad
import System.Random

import Geometry.Circle
import Geometry.Position

data Case = C Int Int Double

instance Show Case where
    show (C na nc sc) = "C " ++ show na ++ " " ++ show nc ++ " " ++ show sc

-- Generate a random scaled circle 
randomScaledCircle :: Double -> IO Circle
randomScaledCircle sc = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return $ Cir (Pos x y) (sc*r)

-- Generate n random circles with the radii scaled by sc
randomScaledCircles :: Int -> Double -> IO [Circle]
randomScaledCircles n sc = do
    cs <- replicateM n $ randomScaledCircle sc
    return $!! cs
