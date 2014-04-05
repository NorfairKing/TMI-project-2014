module Generation where

import Control.Monad
import System.Random
import Text.Printf
import System.IO

import TestCases

{-intPrompt :: String -> IO Int
intPrompt str = do
    putStrLn str 
    putStr "> "
    line <- getLine
    return (read line :: Int)
-}

generateTestCases :: IO ()
generateTestCases = do
    foldr (>>) (return ()) [ generateTestCase nc | nc <- circlesNrs ]

generateTestCase nc = do
    putStrLn $ show nc ++ " generated."
    outh1 <- openFile ("testInput/testcase_1_" ++ show nc ++ ".txt") WriteMode
    outh2 <- openFile ("testInput/testcase_2_" ++ show nc ++ ".txt") WriteMode
    outh3 <- openFile ("testInput/testcase_3_" ++ show nc ++ ".txt") WriteMode
    hPutStrLn outh1 $ show 1
    hPutStrLn outh2 $ show 2
    hPutStrLn outh3 $ show 3
    hPutStrLn outh1 $ show nc
    hPutStrLn outh2 $ show nc
    hPutStrLn outh3 $ show nc
    triples <- replicateM nc getRandomTriple
    mapM_ (putTriple outh1) triples
    mapM_ (putTriple outh2) triples
    mapM_ (putTriple outh3) triples
    hClose outh1
    hClose outh2
    hClose outh3
    where putTriple outh (x,y,r) = hPutStrLn outh $ unwords $ map (printf "%0.15f") [x,y,r]

getRandomTriple :: IO (Double, Double, Double)
getRandomTriple = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (x,y,r)
