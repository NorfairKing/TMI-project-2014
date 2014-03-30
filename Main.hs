module Main where

import Parser
import Geometry
import VectorSpace

import Naive
import Quadratic
import Linearithmic

import Control.Monad
import Control.Monad.State
import Data.Maybe
import System.CPUTime
import Text.Printf


solve :: Int -> [Circle] -> Maybe [Position]
solve a c | a == 1 = Just $ Naive.intersections         c
          | a == 2 = Just $ Quadratic.intersections     c
          | a == 3 = Just $ Linearithmic.intersections  c
solve _ _ = Nothing

-- IO

readCircle :: Loader Circle
readCircle = do
    (x, y, r) <- parseLine
    return (Pos x y, r)

readAlgorithm :: Loader Int
readAlgorithm = parseLine
  
readCircles :: Loader [Circle]
readCircles = do
    nCircles <- parseLine
    replicateM nCircles readCircle

main :: IO ()
main = do
    input <- lines `fmap` getContents
    let (algorithm, circleData) = runState readAlgorithm input
    let circles = runState readCircles circleData
    start <- getCPUTime
    let solution = solve algorithm $ fst circles
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^9)
    case solution of
        Nothing -> putStrLn "Dit algoritme is niet geïmplementeerd."
        Just _  -> do 
                mapM_ print $ fromJust solution
                print $ floor diff
