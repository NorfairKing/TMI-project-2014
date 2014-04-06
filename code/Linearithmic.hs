module Linearithmic where

import Data.Tree.AVL

import Circle
import Position

intersections :: [Circle] -> [Position]
intersections cs = nub $ go (astree ordering $ eventPointss cs) []
    where
      go :: [Circle] -> [Event] -> AVL Circle -> [Position]
      go [e] act = []
      go (Insert (Cir ct r) : evl) act = concatMap (circlesIntersections c) act' ++ go evl (insert c act)
        where
          act' = asListL $ takeLE (ct,max) $ takeGE (ct,min) act -- where magic should happen, get all circles in the interval y - r to y + r
          (min,max) = (ct <-> r', ct <+> r')
          r' = (Pos 0 r)
      go (Delete c : evl) act = go evl (delete c act)
      ordering c1@(Pos _ y1,_) c1@(Pos _ y2,_) = compare y1 y2
