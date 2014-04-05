module Main where

import System.Environment   
import Data.List 
import System.IO 

scale = 1000

printSVGStart :: IO ()
printSVGStart = do
    putStrLn $ "<svg width=\"1920\" height=\"1080\">"
    putStrLn $ "    <rect width=\"100%\" height=\"100%\" fill=\"black\"/>"

printCircle :: (Double, Double, Double) -> IO ()
printCircle (x,y,r) = do
    putStrLn $ "    <circle cx=\"" ++ show x' ++ "\" cy=\"" ++ show y' ++ "\" r=\"" ++ show r' ++ "\" stroke=\"white\" stroke-width=\"1\" style=\"fill-opacity:0.0;stroke-opacity:1\"/>"
    where
        [x',y',r'] = map (floor.(*scale)) [x,y,r]

printSVGEnd :: IO ()
printSVGEnd = do
    putStrLn $ "</svg>"

printIntersection :: (Double, Double) -> IO ()
printIntersection (x,y) = do
    putStrLn $ "    <circle cx=\"" ++ show x' ++ "\" cy=\"" ++ show y' ++ "\" r=\"3\" fill=\"red\" />"
    where
        [x',y'] = map (floor.(*scale)) [x,y]
 
readCircle :: String -> (Double,Double,Double)   
readCircle = triplify . map read . words 
    where triplify = (\[x,y,r] -> (x,y,r))

readResult :: String -> (Double,Double)   
readResult = tuplify . map read . words 
    where tuplify = (\[x,y] -> (x,y))

main = do
    [inputFile,resultsFile] <- getArgs
    handleInput <- openFile inputFile ReadMode 
    handleResults <- openFile resultsFile ReadMode 
    contentsInput <- hGetContents handleInput
    contentsResults <- hGetContents handleResults
    let circleLines = (drop 2 . lines) contentsInput
    let circles = map readCircle circleLines
    let resultLines = (drop 2 . reverse . lines) contentsResults
    let intersections = map readResult resultLines
    printSVGStart
    mapM_ printCircle circles
    mapM_ printIntersection intersections
    printSVGEnd
