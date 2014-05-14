module Benchmark.Solve where

import Control.DeepSeq
import Control.Monad
import Criterion.Measurement
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment
import Benchmark.Experiments
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

doExperiment (DoublingRatioExperiment name ass) = do
    let ofile = figuresDir ++ "/"
                ++ doublingRatioPrefix
                ++ name
                ++ doublingRatioExtension
    outh <- openFile ofile WriteMode
    times <- mapM ( mapM benchAssignmentAvg ) ass
    let ratioss = times `deepseq` map ratios times
    hPutStrLn outh $ latexTable ratioss
    hClose outh

doExperiment (NrIntersectionsExperiment name cases) = do
    let ofile = resultsDir ++ "/"
                ++ experimentPrefix
                ++ name
                ++ experimentExtension
    outh <- openFile ofile WriteMode
    css <- mapM generate cases
    let results = zip cases $ map totalNrIntersections css
    let csvStr = unlines $ map resultToCsv results
    hPutStrLn outh csvStr
    hClose outh
    where 
        generate (n, sc) = randomScaledCircles n sc
        resultToCsv ((n,sc),i) = show n ++ "," ++ show sc ++ "," ++ show i


ratios :: [Double] -> [Double]
ratios [] = []
ratios [_] = []
ratios (t1:t2:ts) = (t2/t1) : ratios (t2:ts)

latexTable :: [[Double]] -> String
latexTable ratioss =
       "\\[\n"
    ++ "\\begin{array}{|c||" ++ replicate (length $ head ratioss) 'c' ++ "|}\n"    
    ++ "\\hline \n"
    ++ "& " ++ concat (intersperse " & " $ map show $ tail drNcs) ++ "\\\\\n"
    ++ "\\hline \\hline \n"
    ++ concatMap row (zip drScs ratioss)
    ++ "\\end{array}\n"
    ++ "\\]\n"
    where 
        row (sc, rs) = printf "%.3f" sc ++ " & " ++ row' rs
        row' [] = ""
        row' [r] = pretty r ++ " \\\\ \\hline \n"
        row' (r:rs) = pretty r ++ " & " ++ row' rs
        pretty d = printf "%.1f" d







