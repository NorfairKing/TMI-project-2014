module Naive where

import VectorSpace
import Geometry

import Data.List

intersections :: [Circle] -> [Position]
intersections [] = []
intersections [c] = []
intersections (c:cs) = (concat [ circlesIntersections c c' | c' <- cs ]) ++ intersections cs
                   
