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

testCircles =
    [
    Cir (Pos x y) r | x <- [1], y <- [1..3], r <- [0.5]
    ]

testMap = [ (circleInterval c, c) | c <- testCircles ]
ivmap = fromList testMap


{-
-- 2 or more circle may have intersections
intersections cs  = A.nub $ -- Remove duplicate positions
                    concat $ -- Concat all computed lists of intersections
                    A.asListL $ -- Convert the tree to a list of lists
                    go (sort $ eventPointss cs) A.empty A.empty -- Go !
    where
        -- The main computing function
        go :: [Event] -> AVL Circle -> AVL Circle -> AVL [Position]

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
    $ takeLE (yOrdering $ Cir max r) -- log N
    $ takeGE (yOrdering $ Cir min r) -- log N
    act 
    where 
        (min,max) = (ct <-> r', ct <+> r')
        max_r = radius $ assertReadR rad
        r' = Pos 0 (r + max_r)

-}
