module Naive where

import Data.List

import Circle
import Position

intersections :: [Circle] -> [Position]
intersections [] = []
intersections [c] = []
intersections l = nub $ concat $ go l
    where 
        go [c] = []
        go (c:cs) = (map (circlesIntersections c) cs) ++ go cs
