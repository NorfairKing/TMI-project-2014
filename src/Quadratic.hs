module Quadratic where

import Data.List

import Event
import Geometry.Circle
import Geometry.Position

intersections :: [Circle] -> [Position]
intersections cs = nub $ go (sort $ eventPointss cs) []
    where
        go [e] act = []
        go (Insert c : evl) act = concatMap (circlesIntersections c) act ++ go evl (c:act)
        go (Delete c : evl) act = go evl (delete c act)
