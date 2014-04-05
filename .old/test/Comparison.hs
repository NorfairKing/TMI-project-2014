module Comparison where

import Control.Monad
import System.IO
import System.Environment 
import Data.List

import Circle
import TestCases

compareTestResults :: IO ()
compareTestResults = do
    foldr (>>) (return ()) [ compareCase nc | nc <- circlesNrs ]

compareCase :: Int -> IO ()
compareCase nc = do
    inhExpected <- openFile ("testResults/testcase_1_" ++ show nc ++ ".txt") ReadMode
    inhActual2  <- openFile ("testResults/testcase_2_" ++ show nc ++ ".txt") ReadMode
    inhActual3  <- openFile ("testResults/testcase_3_" ++ show nc ++ ".txt") ReadMode
    expected <- hGetContents inhExpected 
    actual2  <- hGetContents inhActual2
    actual3  <- hGetContents inhActual3
    let (correct2,total2) = compareResults (lines expected) (lines actual2)
    let (correct3,total3) = compareResults (lines expected) (lines actual3)
    putStrLn $
        if correct2 == total2
            then "2-" ++ show nc ++ ": SUCCESS"
            else "2-" ++ show nc ++ ": failure " ++ show correct2 ++ "/" ++ show total2
    putStrLn $
        if correct3 == total3
            then "3-" ++ show nc ++ ": SUCCESS"
            else "3-" ++ show nc ++ ": failure " ++ show correct3 ++ "/" ++ show total3
    hClose inhExpected
    hClose inhActual2
    hClose inhActual3

-- Compare two lists of results and compute the amount of differences.
compareResults :: [String] -> [String] -> (Int, Int)
compareResults r1 r2 = countDifferences r1' r2'
    where
        r1' = f r1
        r2' = f r2
        f = map (uncurry Res . tuplify . map rd . words) . drop 2 . reverse
        rd x = read x :: Double
        tuplify [x,y] = (x,y)

-- Count the difference between two lists, arranged in any order.
countDifferences :: Eq a => [a] -> [a] -> (Int, Int)
countDifferences rs ss = (correct, total)
    where
        correct = sum $ map fromEnum comb
        total = length comb
        comb = sInR ++ rInS
        rInS = map (`elem` ss) rs
        sInR = map (`elem` rs) ss

