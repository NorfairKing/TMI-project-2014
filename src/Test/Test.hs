module Test.Test where

import System.Directory

import Test.Settings
import Test.Generation
import Test.Execution
import Test.Comparison

test :: [String] -> IO () 
test args = do
    check
    let [command] = args
    case command of
        "generate"  -> generateTests
        "run"       -> runTests
        "compare"   -> compareTests
        "all"       -> do
                generateTests
                runTests
                compareTests

check = do
    bs <- mapM doesDirectoryExist dirs
    mapM_ createDirectory $ select (map not bs) dirs
    where
        dirs = [inputDir, outputDir]

select :: [Bool] -> [a] -> [a]
select [] [] = []
select  _ [] = []
select [] _  = []
select (b:bs) (a:as)
    = if b
    then a : select bs as
    else select bs as
