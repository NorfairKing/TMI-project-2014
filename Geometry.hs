module Geometry where

import VectorSpace

import Data.List

-- Definitions
type Polygon    = [Line]
type Line       = (Position, Position)
type Circle     = (Position, Scalar) 

-- Test whether the rectangles of two lines overlap.
rectangleOverlap :: Line -> Line -> Bool
rectangleOverlap (Pos x11 y11, Pos x12 y12) (Pos x21 y21, Pos x22 y22)
    = and [b1,b2,b3,b4]
    where
        b1 = max x11 x12 >= min x21 x22
        b2 = max x21 x22 >= min x11 x12
        b3 = max y11 y12 >= min y21 y22
        b4 = max y21 y22 >= min y11 y12

-- Test whether two lines intersect
linesIntersect :: Line -> Line -> Bool
linesIntersect l1@(v11,v12) l2@(v21,v22)
    = rectangleOverlap l1 l2
      && x1 * x2 <= 0
      && x3 * x4 <= 0
    where 
        x1 = v21 <-> v11 `x` m1       
        x2 = v22 <-> v11 `x` m1
        x3 = v11 <-> v21 `x` m2       
        x4 = v12 <-> v21 `x` m2
        m1 = v12 <-> v11
        m2 = v22 <-> v21

-- Test whether two circles intersect
circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect (p1,r1) (p2,r2) = 
    n <= r1 + r2 && n >= abs (r1 - r2)
    where n = norm $ p1 <-> p2

-- Give the intersection points of two circles.
-- This works for two distinct circles, with two different intersections
-- This works in O(1) time
-- A prettier version, works the same way.
circlesIntersections :: Circle -> Circle -> [Position]
circlesIntersections c1@ (p1@(Pos x1 y1) ,r1) c2@ (p2@(Pos x2 y2),r2) =
    if circlesIntersect c1 c2 && (c1 /= c2) 
        then nub [ Pos px1 py1, Pos px2 py2 ]
        else []
    where
        px1 = s + 2*(y1-y2)/d'*δ
        px2 = s - 2*(y1-y2)/d'*δ
        py1 = t - 2*(x1-x2)/d'*δ
        py2 = t + 2*(x1-x2)/d'*δ
        s   = ((x1+x2)/2) + (x2-x1)*α
        t   = ((y1+y2)/2) + (y2-y1)*α
        α   = (r1*r1-r2*r2)/(2*d')
        δ   = (1/4) * sqrt ( (d+r1+r2)*(d+r1-r2)*(d-r1+r2)*(r1+r2-d)  )
        d'  = d*d
        d   = norm $ p1 <-> p2
