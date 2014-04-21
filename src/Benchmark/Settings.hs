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

dirs = [resultsDir]
