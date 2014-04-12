module Naive where

import Data.List

import Geometry.Circle
import Geometry.Position

intersections :: [Circle] -> [Position]
intersections [] = []
intersections [c] = []
intersections l = nub $ go l
    where 
        go [] = []
        go [c] = []
        go (c:cs) = concatMap (circlesIntersections c) cs ++ go cs
