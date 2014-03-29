-- We need this for reasons.
{-# LANGUAGE MultiParamTypeClasses #-}

-- Define a euclidean vector space with...
class Vector a b where
    -- Vector addition
    (<+>) :: a -> a -> a
    -- Vector substraction
    (<->) :: a -> a -> a
    -- Scalar multiplication
    (<*>) :: b -> a -> a
    -- Dotproduct
    o :: a -> a -> b

-- Define a 2D position.
data Position = Pos Double Double

-- Declare that positions are vectors with doubles as scalar.
instance Vector Position Double where
    -- Addition
    (<+>) (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)
    -- Substraction 
    (<->) (Pos x1 y1) (Pos x2 y2) = Pos (x1-x2) (y1-y2)
    -- Scalar multiplication
    (<*>) s (Pos x y) = Pos (s*x) (s*y)
    -- Dotproduct
    o (Pos x1 y1) (Pos x2 y2) = x1*x2+y1*y2

-- Define an equality test for a position.
instance Eq Position where
    (==) (Pos x1 y1) (Pos x2 y2) = (x1 == x2) && (y1 == y2)

-- Define the string representation for a position.
instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

-- Define the magnitude of a vector product.
-- Note: This is not an accurate definition of a vector product,
-- but it fits our purposes.
x :: Position -> Position -> Double
x (Pos x1 y1) (Pos x2 y2) = x1*y2 - y1*x2
