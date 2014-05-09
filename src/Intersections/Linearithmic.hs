module Intersections.Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L
import Data.IntervalMap.Strict as I

import Geometry.Circle
import Geometry.Position
import Geometry.Vector
import Intersections.Event

{-
    Calculate all intersections of a list of circles in O((N+S)log(N)) worst case time.
    This is a sweepline algorithm, based on the quadratic algorithm.
    The eventpoints for this algorithm are all circle events of the input circles.

    We go through every eventpoint from bottom to top,
    inserting circles for insert events into the status 
    and deleting cirlces for delete events.

    For every insertion, we calculate the intersections for each circle in the status whose y-wise interval overlaps with that of the current one.

    When we are done with all events, we will have found every intersection.
-}
intersections :: [Circle] -> [Position]

-- 0 or 1 circles have no intersections
intersections []  = []
intersections [_] = []

-- A list of circles may have intersections.
-- Remove all duplactes in a list of intersections found by the 'go' function.
-- Sort the eventpoints of all circles by y coordinate.
-- and perform the 'go' algorithms with an initially empty interval map.
intersections cs = A.nub $ go (L.sortBy eventOrdering' $ eventPointss cs) I.empty
    where
        -- The working function.
        go :: [Event] -> IntervalMap Position Circle -> [Position]
        
        -- The last event is always a delete event, so there won't be any new intersections.
        go [e] _ = []
        
        -- For an insert event, insert the circle into the status interval map
        -- and find the intersections with the circles in the interval map with overlapping y-wise intervals
        go (Insert c : es) act = intersects ++ next
            where
                intersects = L.concatMap (circlesIntersections c) overlapping
                overlapping = L.map snd $ I.intersecting act thisInterval
        
                next = go es newAct
                newAct = I.insert thisInterval c act

                thisInterval = circleInterval c
        
        -- For a delete event, delete the circle from the status interval map and move on.    
        go (Delete c : es) act = go es newAct
            where newAct = I.delete (circleInterval c) act


toIntervalMap :: [Circle] -> IntervalMap Position Circle        
toIntervalMap cs = fromList $ L.map (\c -> (circleInterval c, c)) cs

circleInterval :: Circle -> Interval Position
circleInterval c@(Cir (Pos x y) r) = (ClosedInterval (Pos x (y-r)) (Pos x (y+r)))

