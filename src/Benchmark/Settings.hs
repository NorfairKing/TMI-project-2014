module Benchmark.Settings where

nas, ncs :: [Int]
nas = [ 1..3 ]
ncs = [ c*10^e | e <- [0..1], c<- [1..9] ]

scs :: [Double]
scs = [ 10**x | x <- [(-2)..2 ] ]

ntDefault :: Int
ntDefault = 10

resultsDir = "results"

csvFile na = resultsDir ++ "/" ++ "results_" ++ show na ++ ".csv"
