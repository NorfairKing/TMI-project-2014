module Test.Generation where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Cmd
import System.Directory
import System.IO
import System.Random
import Text.Printf

import Circle
import Parser
import Position

import Test.Settings

-- Generation
generateTests :: IO ()
generateTests = mapM_ generateTestCase circlesNrs

generateTestCase :: Int -> IO ()
generateTestCase nc = do
    circles <- replicateM nc getRandomCircle
    mapM_ (makeFile circles) algorithmNrs

makeFile :: [Circle] -> Int -> IO ()
makeFile cs na = do
    outh <- openFile (
                inputDir
                ++ "/"
                ++ caseName na nc
                ) WriteMode
    hPrint outh na
    hPrint outh nc
    mapM_ (hPrint outh) cs
    hClose outh
    putStrLn $ caseName na nc ++ " generated."
    where nc = length cs


getRandomCircle :: IO Circle
getRandomCircle = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (Cir (Pos x y) r)
