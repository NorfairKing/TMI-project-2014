module Test.BaseTest where

import System.Cmd
import System.IO
import System.Directory
import Control.Monad.State

import Test.Comparison
import Parser
import Geometry.Position

example_input = "../opgave/voorbeeld_invoer.txt"
example_output = "../opgave/voorbeeld_uitvoer.txt"

temp_file = "tmp.txt"

baseTest = do
    system cmd
    actual <- getIntersections temp_file
    expected <- getIntersections example_output  
    let (correct, total) = countDifferences actual expected
    if correct == total
        then do
            putStrLn "BASETEST SUCCESS"
            removeFile temp_file  
        else putStrLn "BASETEST FAILURE"
    where
        cmd = "./Main" ++ " < " ++ example_input ++ " > " ++ temp_file

getIntersections :: String -> IO [Position]
getIntersections file = do
    inh <- openFile file ReadMode
    ll <- (drop 2 . reverse . lines) `fmap` hGetContents inh
    let np = length ll
    let (positions, _) = runState (replicateM np parseLine) ll
    return positions


