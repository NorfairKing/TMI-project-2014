module Benchmark.DoubleRatio where

import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Settings
import Benchmark.Solve

doubleRatioExperiment = do
    rs <- doubleRatio 1
    putStrLn $ unlines $ map (unwords . map show) $ rs

doubleRatio na = do
    mx <- mapM (mapM benchAssignmentAvg) $ drAssignments na
    return $ map ratios $ mx

drAssignments :: Int -> [[Assignment]]
drAssignments na = map (drAssignments' na) drScs

drAssignments' :: Int -> Double -> [Assignment]
drAssignments' na sc = [ (A drAcc (C na nc sc)) | nc <- drNcs]

ratios [] = []
ratios [x] = []
ratios (x:y:ll) = (y/x) : ratios (y:ll)
