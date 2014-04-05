module Naive where

import Data.List

import Circle
import Position

intersections :: [Circle] -> [Position]
intersections [] = []
intersections [c] = []
intersections l = nub $ go l
    where 
        go [] = []
        go [c] = []
        go (c:cs) = concatMap (circlesIntersections c) cs ++ go cs
