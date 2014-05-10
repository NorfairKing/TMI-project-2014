module Benchmark.Solve where

import Control.DeepSeq
import Control.Monad
import Criterion.Measurement
import Data.Maybe
import System.IO
import Text.Printf

import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment
import Benchmark.Settings
import Geometry.Circle
import Geometry.Position
import Intersections.Intersections

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
doExperiment (RawDataExperiment name as) = do
    let ofile = resultsDir ++ "/"
                ++ experimentPrefix
                ++ name
                ++ experimentExtension
    mapM_ (runAssignment' ofile) as

doExperiment ( DoublingRatioExperiment name ass) = do
    let ofile = figuresDir ++ "/"
                ++ doublingRatioPrefix
                ++ name
                ++ doublingRatioExtension
    outh <- openFile ofile WriteMode
    times <- mapM ( mapM benchAssignmentAvg ) ass
    let ratioss = map ratios times
    hPutStrLn outh $ latexTable ratioss
    hClose outh

ratios :: [Double] -> [Double]
ratios [] = []
ratios [_] = []
ratios (t1:t2:ts) = (t2/t1) : ratios (t2:ts)

latexTable :: [[Double]] -> String
latexTable ratioss =
       "\\begin{array}{" ++ replicate (length ratioss) 'c' ++ "}\n"    
    ++ concatMap row ratioss
    ++ "\\end{array}\n"
    where 
        row [] = ""
        row [r] = oneDecimal r ++ "\\\\\n"
        row (r:rs) = oneDecimal r ++ " & " ++ row rs
        oneDecimal d = printf "%.2f" d







