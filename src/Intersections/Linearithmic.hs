module Intersections.Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L
import Data.IntervalMap.Strict as I

import Geometry.Circle
import Geometry.Position
import Geometry.Vector
import Intersections.Event

intersections :: [Circle] -> [Position]
intersections []  = []
intersections [_] = []

intersections cs = A.nub $ go (L.sortBy otherEventOrdering $ eventPointss cs) I.empty
    where
        go :: [Event] -> IntervalMap Position Circle -> [Position]
        
        go [e] _ = []
         
        go (Insert c : es) act = intersects ++ next
            where
                intersects = L.concatMap (circlesIntersections c) overlapping
                overlapping = L.map snd $ I.intersecting act thisInterval
        
                next = go es newAct
                newAct = I.insert thisInterval c act

                thisInterval = circleInterval c
    
        go (Delete c : es) act = go es newAct
            where newAct = I.delete (circleInterval c) act


toIntervalMap :: [Circle] -> IntervalMap Position Circle        
toIntervalMap cs = fromList $ L.map (\c -> (circleInterval c, c)) cs

circleInterval :: Circle -> Interval Position
circleInterval c@(Cir (Pos x y) r) = (ClosedInterval (Pos x (y-r)) (Pos x (y+r)))

