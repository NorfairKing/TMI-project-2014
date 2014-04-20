module Benchmark.Settings where

import Benchmark.Case
import Benchmark.Assignment

nas :: [Int]
nas = [ 1..3 ]

-- Paths
resultsDir = "../analysis/results"
experimentPrefix = "experiment_"
experimentExtension = ".csv"

csvFile na = "results_" ++ show na ++ ".csv"

doubleRatioDir = "../analysis/results_doubleRatio"

drNcs :: [Int]
drNcs = [ 10*2^x | x <- [0..5] ]
drScs = [ 0.001 * 2**x | x <- [1..8]]

drAcc :: Int
drAcc = 10

dirs = [resultsDir,doubleRatioDir]
