data Position = Pos Double Double

-- Define a euclidean vector space with...
class Vector a where
    -- Vector addition
    (<+>) :: a -> a -> a
    -- Vector substraction
    (<->) :: a -> a -> a
    -- Scalar multiplication
    (<*>) :: Double -> a -> a
    -- Dotproduct
    o :: a -> a -> Double

instance Vector Position where
    -- Addition
    (<+>) (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)
    -- Substraction 
    (<->) (Pos x1 y1) (Pos x2 y2) = Pos (x1-x2) (y1-y2)
    -- Scalar multiplication
    (<*>) s (Pos x y) = Pos (s*x) (s*y)
    -- Dotproduct
    o (Pos x1 y1) (Pos x2 y2) = x1*x2+y1*y2

