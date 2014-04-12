module Visual.Visual where

import Control.Monad.State
import System.Environment
import System.IO

import Geometry.Circle
import Geometry.Position
import Parser
import Visual.SVG

visualize :: [String] -> IO ()
visualize args = do
    
    hanCircles <- openFile circleFile ReadMode
    hanIntersections <- openFile intersectionsFile ReadMode

    circles <- getCircles hanCircles
    intersections <- getIntersections hanIntersections

    putStrLn $ drawing circles intersections

    hClose hanCircles
    hClose hanIntersections

    where 
        [circleFile, intersectionsFile] = args


drawing :: [Circle] -> [Position] -> String
drawing cs is = unlines $
                   [draw Header]
                ++ [draw Background]
                ++  map draw cs
                ++  map draw is
                ++ [draw Footer]

getCircles :: Handle -> IO [Circle]
getCircles han = do
    ll <- (tail. lines) `fmap`  hGetContents han
    let (nc, rest) = runState parseLine ll
    let (circles, _) = runState (replicateM nc parseLine) rest
    return circles


getIntersections :: Handle -> IO [Position]
getIntersections han = do
    ll <- (drop 2 . reverse . lines) `fmap` hGetContents han
    let np = length ll
    let (positions, _) = runState (replicateM np parseLine) ll
    return positions
