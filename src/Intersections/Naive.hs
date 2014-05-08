module Intersections.Naive where

import Data.List

import Geometry.Circle
import Geometry.Position

{-
    Calculate all intersections of a list of circles in O(n^2) time. (n*(n-1)/2)
    
    Calculate the intersections of every circle with every other circle.
-}
intersections :: [Circle] -> [Position]

-- 0 circles have no intersections.
intersections [] = []

-- 1 circle has no intersections.
intersections [_] = []

-- A list of circles may have intersections.
intersections l = nub $ go l
    where 
        go [] = []
        go [_] = []
        -- find the intersections of the first circle with every other circles,
        -- then go on to the next circle and put all intersections together in a list.
        go (c:cs) = concatMap (circlesIntersections c) cs ++ go cs
