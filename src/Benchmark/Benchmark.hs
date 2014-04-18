module Benchmark.Benchmark where

import System.Directory

import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Settings
import Benchmark.Solve

benchmark = do
    check
    mapM_ runAssignment assignments

check = do
    b <- doesDirectoryExist resultsDir
    if not b
    then createDirectory resultsDir    
    else return ()
