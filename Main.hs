module Main where

import Control.Monad
import Control.Monad.State
import CrazyParser
import Geometry
import VectorSpace
import System.CPUTime
import Text.Printf

naive :: [Circle] -> [Position]
naive circles =
  error "Dit algoritme is niet geïmplementeerd."

scanline_quadratic :: [Circle] -> [Position]
scanline_quadratic circles =
  error "Dit algoritme is niet geïmplementeerd."

scanline_linearithmic :: [Circle] -> [Position]
scanline_linearithmic circles =
  error "Dit algoritme is niet geïmplementeerd."


solve :: Int -> [Circle] -> [Position]
solve a c | a == 1 = naive c
          | a == 2 = scanline_quadratic c
          | a == 3 = scanline_linearithmic c

readCircle :: Loader Circle
readCircle = do
  (x, y, r) <- parseLine
  return (Pos x y, r)

readAlgorithm :: Loader Int
readAlgorithm = do
  algo <- parseLine
  return algo
  
readCircles :: Loader [Circle]
readCircles = do
  nCircles <- parseLine
  replicateM nCircles readCircle

main :: IO ()
main = do
  input <- lines `fmap` getContents
  let (algorithm, circleData) = runState readAlgorithm input
  let circles = runState readCircles circleData
  mapM_ putStrLn $ map show circles
  let solution = solve algorithm $ fst circles
  mapM_ putStrLn $ map show solution
