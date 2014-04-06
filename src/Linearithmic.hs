module Linearithmic where

import Data.COrdering
import Data.Tree.AVL as A
import Data.List as L

import Circle
import Event
import Position
import Vector

intersections :: [Circle] -> [Position]
intersections cs = L.nub $ concat $ A.asListL $ go (sort $ eventPointss cs) A.empty 0
    where
      go :: [Event] -> AVL Circle -> Double -> AVL [Position]
      go [e] act _ = A.empty
      go (Insert c@(Cir _ r) : evl) act rad = (intersects c) `A.join` next
        where
          intersects :: Circle -> AVL [Position]
          intersects c = A.map (circlesIntersections c) iv
          iv = case r >= rad of
            True -> (interval c act r)
            False -> (interval c act rad)
          next = case r >= rad of
            True -> go evl (A.push (cordering c) c act) r
            False -> go evl (A.push (cordering c) c act) rad
      go (Delete c : evl) act rad = go evl (A.delete (ordering c) act) rad

ordering :: Circle -> Circle -> Ordering 
ordering (Cir (Pos _ y1) _) (Cir (Pos _ y2) _) = compare y1 y2


cordering :: Circle -> Circle -> COrdering Circle
cordering c1 c2 = fstByCC ordering c1 c2

interval :: Circle -> AVL Circle -> Double ->AVL Circle
interval (Cir ct r) act max_r = takeLE (ordering $ Cir max r) $ takeGE (ordering $ Cir min r) act
                      where (min,max) = (ct <-> r', ct <+> r')
                            r' = (Pos 0 (r + max_r))
