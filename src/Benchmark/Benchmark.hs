module Benchmark.Benchmark where

import System.Directory

import Benchmark.Experiments
import Benchmark.Settings
import Benchmark.Solve
import Benchmark.DoubleRatio

benchmark = do
    check
    mapM_ doExperiment allExperiments

-- Create nececary directories
check = do
    bs <- mapM doesDirectoryExist dirs
    mapM_ createDirectory $ select (map not bs) dirs

select :: [Bool] -> [a] -> [a]
select [] [] = []
select  _ [] = []
select [] _  = []
select (b:bs) (a:as)
    = if b
    then a : select bs as
    else select bs as
