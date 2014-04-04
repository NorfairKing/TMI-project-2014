module Naive where

import VectorSpace
import Geometry

import Data.List

intersections :: [Circle] -> [Position]
intersections [] = []
intersections [c] = []
intersections l = nub $ go l
                       where go (c:cs) = (concat [ circlesIntersections c c' | c' <- cs ]) ++ go cs
