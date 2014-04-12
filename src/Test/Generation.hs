module Test.Generation where

import Control.Monad.State
import System.IO
import System.Random

import Geometry.Circle
import Geometry.Position
import Parser
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
