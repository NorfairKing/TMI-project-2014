module Benchmark.Settings where

nas, ncs :: [Int]
nas = [ 1..3 ]
ncs = [ c*10^e | e <- [0..2], c<- [1..9] ]

scs :: [Double]
scs = [ 10**x | x <- [(-2)..2 ] ]

ntDefault :: Int
ntDefault = 100

resultsDir = "results"

csvFile na = "results_" ++ show na ++ ".csv"
