-- Definitions
type Scalar = Double
type Vector = (Scalar, Scalar)
type Line = (Vector, Vector)
type Polygon = [Line]

-- The vector addition
vplus :: Vector -> Vector -> Vector
vplus (x1,y1) (x2,y2) = ((x1+x2),(y1+y2))

-- The vector substraction
vmin :: Vector -> Vector -> Vector
vmin (x1,y1) (x2,y2) = ((x1-x2),(y1-y2))

-- The vector dotproduct 
vdot :: Scalar -> Vector -> Vector
vdot s (x,y) = (s*x, s*y) 

-- The magnitude of the vector product of two vectors
x :: Vector -> Vector -> Scalar
x (x1,y1) (x2,y2) = x1*y2 - y1*x2

-- Test whether the rectangles of two lines overlap.
-- TODO Yes, this is described poorly...
rectangleOverlap :: Line -> Line -> Bool
rectangleOverlap ((x11,y11),(x12,y12)) ((x21,y21),(x22,y22))
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
        x1 = (v21 `vmin` v11) `x` m1       
        x2 = (v22 `vmin` v11) `x` m1
        x3 = (v11 `vmin` v21) `x` m2       
        x4 = (v12 `vmin` v21) `x` m2
        m1 = (v12 `vmin` v11)
        m2 = (v22 `vmin` v21)
