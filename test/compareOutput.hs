import System.IO
import System.Environment 

import Data.List

precision = 10**(-12)

data Result = Res Double Double

instance Show Result where
    show (Res x y) = "Res " ++ show x ++ show y

instance Eq Result where
    (==) (Res x1 y1) (Res x2 y2) = abs(x1-x2) < precision
                                    && abs(y1-y2) < precision


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

main :: IO ()
main = do
    [expected, actual] <- getArgs
    handle1 <- openFile expected ReadMode  
    expectedResults <- hGetContents handle1 
    handle2 <- openFile actual ReadMode  
    actualResults <- hGetContents handle2 
    let (correct,total) = compareResults (lines expectedResults) (lines actualResults)
    putStrLn $
        if correct == total
            then "Test Success: " ++ show correct ++ "/" ++ show total ++ " passed."
            else "Test Failure: " ++ show correct ++ "/" ++ show total ++ " passed."
    hClose handle1 
    hClose handle2

