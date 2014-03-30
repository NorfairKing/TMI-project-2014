import System.IO
import System.Environment 

import Data.List

precision = 10**(-15)

-- Compare two lists of results and compute the amount of differences.
compareResults :: [String] -> [String] -> Int
compareResults r1 r2 = countDifferences r1' r2' 0
    where
        r1' = f r1
        r2' = f r2
        f = sort . drop 2 . reverse

-- Count the amount of differences between two lists of points.
countDifferences [] [] acc = acc
countDifferences rs [] acc = acc + length rs
countDifferences [] ss acc = acc + length ss
countDifferences (r:rs) (s:ss) acc = countDifferences rs ss acc'
    where
        acc' = acc + fromEnum ( dist > precision )
        dist = sqrt((d11-d21)**2 + (d12-d22)**2)
        [d11, d12] = map rd $ words r
        [d21, d22] = map rd $ words s
        rd x = read x :: Double

main :: IO ()
main = do
    [expected, actual] <- getArgs
    handle1 <- openFile expected ReadMode  
    expectedResults <- hGetContents handle1 
    handle2 <- openFile actual ReadMode  
    actualResults <- hGetContents handle2 
    let nTests = show $ (length . lines)  expectedResults - 2
    let differences = compareResults (lines expectedResults) (lines actualResults)
    putStrLn $
        if differences == 0
            then "Test Succes: " ++ nTests ++ "/" ++ nTests ++ " passed."
            else "Test Failure: " ++ show differences ++ "/" ++ nTests ++ " failed."
    hClose handle1 
    hClose handle2
