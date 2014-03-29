module Geometry where
import VectorSpace
-- Definitions
type Polygon    = [Line]
type Line       = (Position, Position)
type Circle     = (Position, Scalar)

{-              
-- Euclidean distance!
distance :: Position -> Position -> Double
distance (Position x1 y1) (Position x2 y2) = sqrt (x'*x' + y'*y')
  where
    x' = x1 - x2
    y' = y1 - y2


-- Check whether two circles intersect
intersect :: Circle -> Circle -> Bool
intersect (c1,r1) (c2,r2) = r1 + r2 <= distance c1 c2
-}

-- Test whether the rectangles of two lines overlap.
-- TODO Yes, this is described poorly...
rectangleOverlap :: Line -> Line -> Bool
rectangleOverlap ((Pos x11 y11),(Pos x12 y12)) ((Pos x21 y21),(Pos x22 y22))
    = and [b1,b2,b3,b4]
    where
        b1 = max x11 x12 >= min x21 x22
        b2 = max x21 x22 >= min x11 x12
        b3 = max y11 y12 >= min y21 y22
        b4 = max y21 y22 >= min y11 y12

-- Test whether two lines intersect
lineIntersects :: Line -> Line -> Bool
lineIntersects l1@(v11,v12) l2@(v21,v22)
    = rectangleOverlap l1 l2
      && x1 * x2 <= 0
      && x3 * x4 <= 0
    where 
        x1 = (v21 <-> v11) `x` m1       
        x2 = (v22 <-> v11) `x` m1
        x3 = (v11 <-> v21) `x` m2       
        x4 = (v12 <-> v21) `x` m2
        m1 = (v12 <-> v11)
        m2 = (v22 <-> v21)
