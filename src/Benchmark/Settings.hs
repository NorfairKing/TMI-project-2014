module Benchmark.Settings where

nas, ncs :: [Int]
nas = [ 1..3 ]
ncs = [ 10,20..200 ]

scs :: [Double]
scs = [ 10**x | x <- [(-2)..2] ]

ntDefault :: Int
ntDefault = 10

resultsDir = "results"

csvFile na = "results_" ++ show na ++ ".csv"
