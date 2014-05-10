module Benchmark.Settings where

import Benchmark.Case
import Benchmark.Assignment

nas :: [Int]
nas = [ 1..3 ]

-- Paths
resultsDir = "../analysis/results"
experimentPrefix = "experiment_"
experimentExtension = ".csv"

figuresDir = "../verslag/illustraties"
doublingRatioPrefix = "table_"
doublingRatioExtension = ".tex"

csvFile na = "results_" ++ show na ++ ".csv"

dirs = [resultsDir]
