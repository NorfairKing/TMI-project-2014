module Intersections.Intersections where

import Control.DeepSeq
import Control.Monad.State
import Criterion.Measurement
import Data.Maybe
import System.CPUTime

import Geometry.Circle
import Geometry.Position
import qualified Intersections.Linearithmic as Linearithmic
import qualified Intersections.Naive as Naive
import qualified Intersections.Quadratic as Quadratic
import Parser
import Settings

intersections :: IO ()
intersections = do
    (algorithm, circles) <- getInput

    (t, solution) <- time $ ioSolve algorithm circles
   
    if algorithm `elem` nas
    then do
        mapM_ print solution
        putStrLn "" 
        print $ floor (t * 1000)
    else putStrLn "Dit algoritme is niet geïmplementeerd."

getInput :: IO (Int, [Circle])
getInput = do
    input <- lines `fmap` getContents
    let (algorithm, circleData) = runState readAlgorithm input
    let (circles, _) = runState readCircles circleData
    return $!! (algorithm, circles)

readAlgorithm :: Loader Int
readAlgorithm = parseLine

readCircles :: Loader [Circle]
readCircles = do
    nCircles <- parseLine
    replicateM nCircles parseLine

-- Make a forced IO action out of the main solve function.
ioSolve :: Int -> [Circle] -> IO [Position]
ioSolve na cs | na `elem` nas 
    -- $!! ensures that the right side is deeply evaluated before it's returned.
    = return $!! filter realPos $ solve na cs
ioSolve _ _ = return []

solve :: Int -> [Circle] -> [Position]
solve 1 c = Naive.intersections         c
solve 2 c = Quadratic.intersections     c
solve 3 c = Linearithmic.intersections  c
solve _ _ = []
