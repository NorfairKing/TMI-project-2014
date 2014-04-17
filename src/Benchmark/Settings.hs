module Benchmark.Settings where

nas, ncs :: [Int]
nas = [ 1..3 ]
ncs = [ 10,15..100 ]

scs :: [Double]
scs = [0.1,0.15..1]

ntDefault :: Int
ntDefault = 1

resultsDir = "../analysis/results"

csvFile na = "results_" ++ show na ++ ".csv"
