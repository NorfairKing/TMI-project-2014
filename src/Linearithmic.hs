module Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L

import Circle
import Event
import Position
import Vector

intersections :: [Circle] -> [Position]
intersections cs = L.nub $ concat $ A.asListL $ go (sort $ eventPointss cs) A.empty
    where
      go :: [Event] -> AVL Circle -> AVL [Position]
      go [e] act = A.empty
      go (Insert c : evl) act = (intersects c) `A.join` next
        where
          intersects :: Circle -> AVL [Position]
          intersects c = A.map (circlesIntersections c) (interval c act)
          next = go evl (A.push (cordering c) c act)
      go (Delete c : evl) act = go evl (A.delete (ordering c) act)

ordering :: Circle -> Circle -> Ordering 
ordering (Cir (Pos _ y1) _) (Cir (Pos _ y2) _) = compare y1 y2


cordering :: Circle -> Circle -> COrdering Circle
cordering c1 c2 = fstByCC ordering c1 c2

interval :: Circle -> AVL Circle -> AVL Circle
interval (Cir ct r) act = takeLE (ordering $ Cir max r) $ takeGE (ordering $ Cir min r) act
                      where (min,max) = (ct <-> r', ct <+> r')
                            r' = (Pos 0 (r + maxRadius act))

maxRadius :: AVL Circle -> Double
maxRadius act = maximum [r | (Cir _ r) <- asListL act ]
