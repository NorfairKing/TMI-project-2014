module Intersections.Quadratic where

import Data.List as L
import Data.Tree.AVL as A

import Geometry.Circle
import Geometry.Position
import Intersections.Event

{-
    Calculate all intersections of a list of circles in O(n^2) worst case time.
    
    This is a sweepline algorithm.
    The eventpoints for this algorithms are all circle events of the input circles.
    
    We go through every eventpoint from bottom to top,
    inserting circles for insert events into the status
    and deleting cirlces for delete events.

    For every insertion, we calculate the intersections for each circle in the status queue
    
    When we are done with all events, we will have found every intersection.
-}
intersections :: [Circle] -> [Position]

-- 0 or 1 circles have no intersections.
intersections [] = []
intersections [c] = []

-- A list of circles may have intersections.
-- Remove all duplicates in a list of intersections found by the 'go' function.
-- Sort the eventpoints of all circles
-- and perform the 'go' algorithm with an initially empty eventqueue.
intersections cs = A.nub $ go (sort $ eventPointss cs) []
    where
        -- The working function.
        go :: [Event] -> [Circle] -> [Position]
        -- The last event is always a delete event, so there won't be any new intersections.
        go [e] act = []
            
        -- For an insert event, insert the circle into the status queue
        -- and find the intersections with the circles in the eventqueue.
        go (Insert c : evl) act = concatMap (circlesIntersections c) act ++ go evl (c:act)
        
        -- For a delete event, delete the circle from the status queue and move on.
        go (Delete c : evl) act = go evl (L.delete c act)
