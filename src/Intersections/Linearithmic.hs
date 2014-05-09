module Intersections.Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L

import Geometry.Circle
import Geometry.Position
import Geometry.Vector
import Intersections.Event


-- All intersections of a list of circles
intersections :: [Circle] -> [Position]

-- 0 or 1 circles have no intersections
intersections []  = []
intersections [_] = []

-- 2 or more circle may have intersections
intersections cs  = A.nub $ -- Remove duplicate positions
                    concat $ -- Concat all computed lists of intersections
                    A.asListL $ -- Convert the tree to a list of lists
                    go (sort $ eventPointss cs) A.empty A.empty -- Go !
    where
        -- The main computing function
        go :: [Event] -> AVL Circle -> AVL Circle -> AVL [Position]

        -- The last event is always a delete events
        -- there are no more intersections to be found at this point.
        go [e] act _ = A.empty
        
        -- At an insertion event:
        go (Insert c : evl) act rad = intersects c `A.join` next
            where
                -- Intersections for the current circle
                intersects :: Circle -> AVL [Position]
                intersects c = A.map (circlesIntersections c) (interval c act rad)                       
                
                -- Go onto the next circle
                next = go evl (A.push (yCOrdering c) c act) (A.push (rCOrdering c) c rad)

        -- At a deletion event:
        go (Delete c : evl) act rad = go evl (A.delete (yOrdering c) act) (A.delete (rOrdering c) rad)

-- Take all intervals that overlap in the y-direction
interval :: Circle -> AVL Circle -> AVL Circle -> AVL Circle
interval c@(Cir ct r) act rad
    = A.filter (yOverlap c) 
    $ takeLE (yOrdering $ Cir max r) 
    $ takeGE (yOrdering $ Cir min r) act
    where 
        (min,max) = (ct <-> r', ct <+> r')
        max_r = radius $ assertReadR rad
        r' = Pos 0 (r + max_r)


