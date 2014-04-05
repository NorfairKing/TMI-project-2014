module Intersections where

import Control.Monad.State
import Data.Maybe
import System.CPUTime

import Naive
import Quadratic
import Linearithmic

import Parser
import Circle
import Position

intersections :: IO ()
intersections = do
    input <- lines `fmap` getContents
    let (algorithm, circleData) = runState readAlgorithm input
    let (circles, _) = runState readCircles circleData
    
    start <- getCPUTime
    let solution = solve algorithm circles
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^9)
    
    case solution of
        Nothing -> putStrLn "Dit algoritme is niet geïmplementeerd."
        Just _  -> do
                mapM_ print $ fromJust solution
                putStrLn ""
                print $ floor diff

quietIntersections :: IO ()
quietIntersections = do
    input <- lines `fmap` getContents
    let (algorithm, circleData) = runState readAlgorithm input
    let (circles, _) = runState readCircles circleData
    let solution = solve algorithm circles
    case solution of
        Nothing -> putStrLn "Dit algoritme is niet geïmplementeerd."
        Just _  -> do
                putStrLn $ (show (length $ fromJust solution)) ++ " intersections found"

readAlgorithm :: Loader Int
readAlgorithm = parseLine

readCircles :: Loader [Circle]
readCircles = do
    nCircles <- parseLine
    replicateM nCircles parseLine

solve :: Int -> [Circle] -> Maybe [Position]
solve 1 c = Just $ Naive.intersections         c
solve 2 c = Just $ Quadratic.intersections     c
-- Uncomment this when we implement this algorithm
--solve 3 c = Just $ Linearithmic.intersections  c
solve _ _ = Nothing
