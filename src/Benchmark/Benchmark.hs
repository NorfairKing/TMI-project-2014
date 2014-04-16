module Benchmark.Benchmark where

import Benchmark.Settings
import Control.DeepSeq
import Control.Monad
import Criterion.Measurement
import Data.Maybe
import Geometry.Circle
import Geometry.Position
import Intersections.Intersections
import System.Directory
import System.Random
import Text.CSV

instance NFData Position
instance NFData Circle

benchmark = do
    check
    putStrLn "hi"

check = do
    b <- doesDirectoryExist resultsDir
    if b
    then createDirectory resultsDir    
    else return ()

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

-- Make a forced IO action out of the main solve function.
ioSolve :: Int -> [Circle] -> IO [Position]
ioSolve na cs | na `elem` nas
    -- $!! ensures that the right side is deeply evaluated before it's returned.
    = return $!! (fromJust . solve na) cs
ioSolve _ _ = return []


-- Datatypes for benchmarking
data Assignment = A Int Case
data Case = C Int Int Double

-- Generate a random input, solve it, and return the time it took.
benchSolve :: Case -> IO Double
benchSolve (C na nc sc) = do
    cs <- randomScaledCircles nc sc
    cs `deepseq` time_ $ ioSolve na cs

-- Time a specific case multiple times.
benchSolveN :: Assignment -> IO [Double]
benchSolveN (A nt c) = replicateM nt $ benchSolve c

-- Time and output the results to a csv file.
timeToCsv :: Assignment -> IO String
timeToCsv a@(A nt (C na nc sc)) = do
    ts <- benchSolveN a
    return $ printCSV $ map toStr ts
    where toStr t = [show na, show nc, show sc, show t]

runAssignment :: Assignment -> IO ()
runAssignment a@(A _ (C na _ _)) = do
    csv <- timeToCsv a
    appendFile (csvFile na) csv
   
assignments = [ A ntDefault (C na nc sc) | na <- nas, nc <- ncs, sc <- scs ] 
