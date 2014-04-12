module Test.Comparison where

import Control.Monad.State

import System.IO

import Position
import Circle
import Parser


import Test.Settings

compareTests :: IO ()
compareTests = mapM_ compareTest circlesNrs

compareTest :: Int -> IO ()
compareTest nc = do
    expected:results <- mapM (getResults nc) algorithmNrs
    let diffs = zip [2..] $ map (countDifferences expected) results
    mapM_ putOutcome diffs
    where 
        putOutcome (na , (correct, total))
            = putStrLn $ 
                if correct == total
                then caseStr ++ " SUCCESS " ++ ratStr
                else caseStr ++ " FAILURE " ++ ratStr
            where
                ratStr = show correct ++ "/" ++ show total
                caseStr = caseName na nc 
                
getResults :: Int -> Int -> IO [Position]
getResults nc na = do
    inh <- openFile (
            outputDir
            ++ "/" 
            ++ caseName na nc
            ) ReadMode
    ll <- (drop 2 . reverse . lines) `fmap` hGetContents inh
    let np = length ll
    let (positions, _) = runState (replicateM np parseLine) ll
    return positions
    
-- Count the difference between two lists, arranged in any order.
countDifferences :: Eq a => [a] -> [a] -> (Int, Int)
countDifferences rs ss = (correct, total) 
    where
        correct = sum $ map fromEnum comb
        total = length comb 
        comb = sInR ++ rInS 
        rInS = map (`elem` ss) rs
        sInR = map (`elem` rs) ss
