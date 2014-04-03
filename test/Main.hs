module Main where

import System.Environment 

import Generation
import Comparison
import Run

main = do
    [action] <- getArgs 
    case action of
        "generate" -> generateTestCases
        "compare"  -> compareTestResults
        "run"      -> runAll
        "test"     -> do
                generateTestCases
                runAll
                compareTestResults
