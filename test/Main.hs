module Main where

import System.Environment 

import Generation
import Comparison
import Run

main = do
    [action] <- getArgs 
    case action of
        "generate" -> generateTestCases
        "run"      -> runAll
        "compare"  -> compareTestResults
        "test"     -> do
                generateTestCases
                runAll
                compareTestResults
