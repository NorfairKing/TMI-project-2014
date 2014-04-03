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
    foldr (>>) (return ()) [ generateTestCase na nc | na <- algorithmNrs, nc <- circlesNrs ]

generateTestCase na nc = do
    outh <- openFile ("testInput/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt") WriteMode
    print na
    print nc
    triples <- replicateM nc getRandomTriple
    let str = unwords 
    mapM_ (putTriple outh) triples
    hClose outh
    where putTriple outh (x,y,r) = hPutStrLn outh $ unwords $ map (printf "%0.15f") [x,y,r]

getRandomTriple :: IO (Double, Double, Double)
getRandomTriple = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (x,y,r)
