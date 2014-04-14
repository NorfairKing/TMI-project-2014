module Test.Execution where

import Data.List
import System.Directory
import System.Cmd

import Test.Settings

runTests = do
    files <- (drop 2 . sort) `fmap` getDirectoryContents inputDir
    mapM_ runTest files
    
runTest file = do
    system $ cmd file
    putStrLn $ file ++ " done."
    
cmd file = "./Main" ++ " " ++ input ++ " " ++ output
    where
        input = "< "
            ++ inputDir
            ++ "/" 
            ++ file
        output = "> "
            ++ outputDir
            ++ "/" 
            ++ file
