module Naive where

import VectorSpace
import Geometry

import Data.List

intersections :: [Circle] -> [Position]
intersections cs = (nub.concat) [ circlesIntersections c1 c2 | c1 <- cs, c2 <- cs ]
