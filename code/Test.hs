module Test where

import Control.Monad
import System.Random
import System.IO

import Circle
import Position


algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [ c*10^e | e <- [0..1], c<- [1..9] ]


test :: [String] -> IO () 
test args = do
    generateTests

generateTests :: IO ()
generateTests = do
    foldr (>>) (return ()) (map generateTestCase circlesNrs)

generateTestCase :: Int -> IO ()
generateTestCase nc = do
    circles <- replicateM nc getRandomCircle
    mapM_ (makeFile circles) algorithmNrs

makeFile cs na = do
    outh <- openFile ("test_input/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt") WriteMode
    hPutStrLn outh $ show na
    hPutStrLn outh $ show nc
    mapM_ (hPutStrLn outh . show) cs
    hClose outh
    where nc = length cs


getRandomCircle :: IO Circle
getRandomCircle = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (Cir (Pos x y) r)
