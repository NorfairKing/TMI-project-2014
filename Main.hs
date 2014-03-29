module Main where

import Control.Monad
import CrazyParser
import Geometry
import VectorSpace
import System.CPUTime
import Text.Printf

naive :: [Circle] -> [Position]
naive circles =
  return "Dit algoritme is niet geïmplementeerd."

scanline_quadratic :: [Circle] -> [Position]
scanline_quadratic circles =
  return "Dit algoritme is niet geïmplementeerd."

scanline_linearithmic :: [Circle] -> [Position]
scanline_linearithmic circles =
  return "Dit algoritme is niet geïmplementeerd."


mkCircle :: Scalar -> Scalar -> Scalar -> IO Circle
mkCircle x y r = return ((x,y),r)

solve :: Int -> [Circle] -> [Point]
solve a c | a == 1 = naive c
          | a == 2 = scanline_quadratic c
          | a == 3 = scanline_linearithmic c

main = do
  algorithm <- parseLine
  nCircles <- parseLine
  circles <- replicateM nCircles parseLine
  map mkCircle circles
  start <- getCPUTime
  solution <- solve algorithm circles
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^9)
  putStrLn solution
  printf "Computation time: %0.3f ms\n" (diff :: Double)
  
