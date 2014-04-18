module Benchmark.Settings where

import Benchmark.Case
import Benchmark.Assignment

nas, ncs :: [Int]
nas = [ 1..3 ]
ncs = [ 10..100 ]

scs :: [Double]
scs = [0.75]

ntDefault :: Int
ntDefault = 1

resultsDir = "../analysis/results"

csvFile na = "results_" ++ show na ++ ".csv"

assignments = [ A ntDefault (C na nc sc) | nc <- ncs, na <- nas, sc <- scs ]

doubleRatioDir = "../analysis/results_doubleRatio"

drNcs :: [Int]
drNcs = [ 10*2^x | x <- [0..3] ]
drScs = [ 0.0, 0.1 .. 2]

