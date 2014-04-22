module Benchmark.Solve where

import Control.DeepSeq
import Control.Monad
import Criterion.Measurement
import Data.Maybe
import System.IO

import Benchmark.Assignment
import Benchmark.Experiment
import Benchmark.Case
import Benchmark.Settings
import Geometry.Circle
import Geometry.Position
import Intersections.Intersections

instance NFData Position
instance NFData Circle

-- Make a forced IO action out of the main solve function.
ioSolve :: Int -> [Circle] -> IO [Position]
ioSolve na cs | na `elem` nas
    -- $!! ensures that the right side is deeply evaluated before it's returned.
    = return $!! (fromJust . solve na) cs
ioSolve _ _ = return []

benchCase :: Case -> IO Double
benchCase (C na nc sc) = do
    cs <- randomScaledCircles nc sc
    cs `deepseq` time_ $ ioSolve na cs

benchAssignment :: Assignment -> IO [Double]
benchAssignment (A nt c) = replicateM nt $ benchCase c

benchAssignmentAvg :: Assignment -> IO Double
benchAssignmentAvg a = do
    rs <- benchAssignment a
    return $ (sum rs) / (fromIntegral (length rs))

-- Time and output the results to a csv file.
timeToCsv :: Assignment -> IO String
timeToCsv a@(A nt (C na nc sc)) = do
    t <- benchAssignmentAvg a
    return $ recordToStr na nc sc t

recordToStr :: Int -> Int -> Double -> Double -> String
recordToStr na nc sc t
    = show na
    ++ ","
    ++ show nc
    ++ ","
    ++ show sc
    ++ ","
    ++ show (floor (t*1000000)) -- convert to us

runAssignment' :: String -> Assignment -> IO ()
runAssignment' ofile a@(A _ (C na _ _))= do
    csv <- timeToCsv a
    outh <- openFile ofile AppendMode
    hPutStrLn outh csv
    hClose outh

-- Experiments
doExperiment :: Experiment -> IO ()
doExperiment (E name as) = do
    let ofile = resultsDir ++ "/"
                ++ experimentPrefix
                ++ name
                ++ experimentExtension
    mapM_ (runAssignment' ofile) as
