data Point = Point Double Double

-- Define a euclidean vector space with...
class Vector a where
    -- Vector addition
    (<+>) :: a -> a -> a
    -- Scalar multiplication
    (<*>) :: Double -> a -> a
    -- Dotproduct
    o :: a -> a -> Double

instance Vector Point where
    (<+>) (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)
    (<*>) s (Point x y) = Point (s*x) (s*y)
    o (Point x1 y1) (Point x2 y2) = x1*x2+y1*y2

