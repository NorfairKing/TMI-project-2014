module Intersections.Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L

import Geometry.Circle
import Geometry.Position
import Geometry.Vector
import Intersections.Event



intersections :: [Circle] -> [Position]

intersections []  = []
intersections [_] = []
intersections cs  = A.nub $ concat $ A.asListL $ go (sort $ eventPointss cs) A.empty A.empty
    where
      go :: [Event] -> AVL Circle -> AVL Circle -> AVL [Position]
      go [e] act _ = A.empty
      go (Insert c : evl) act rad = intersects c `A.join` next
        where
          intersects :: Circle -> AVL [Position]
          intersects c = A.map (circlesIntersections c) (interval c act rad)
          next = go evl (A.push (cordering c) c act) (A.push (cordering' c) c rad)
      go (Delete c : evl) act rad = go evl (A.delete (ordering c) act) (A.delete (ordering' c) rad)

ordering :: Circle -> Circle -> Ordering 
ordering (Cir (Pos _ y1) _) (Cir (Pos _ y2) _) = compare y1 y2

ordering' :: Circle -> Circle -> Ordering 
ordering' (Cir _ r1) (Cir _ r2) = compare r1 r2

cordering :: Circle -> Circle -> COrdering Circle
cordering = fstByCC ordering

cordering' :: Circle -> Circle -> COrdering Circle
cordering' = fstByCC ordering'

interval :: Circle -> AVL Circle -> AVL Circle ->AVL Circle
interval c@(Cir ct r) act rad = A.filter (overlapY c) $ takeLE (ordering $ Cir max r) $ takeGE (ordering $ Cir min r) act
                      where (min,max) = (ct <-> r', ct <+> r')
                            max_r = radius $ assertReadR rad
                            r' = Pos 0 (r + max_r)

overlapY :: Circle -> Circle -> Bool
overlapY (Cir (Pos _ y1) r1) (Cir (Pos _ y2) r2) = abs(r2+r1) > abs(y2-y1)
