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

-- Define a position
data Position = Pos Double Double

-- Declare that positions are vectors
instance Vector Position where
    -- Addition
    (<+>) (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)
    -- Substraction 
    (<->) (Pos x1 y1) (Pos x2 y2) = Pos (x1-x2) (y1-y2)
    -- Scalar multiplication
    (<*>) s (Pos x y) = Pos (s*x) (s*y)
    -- Dotproduct
    o (Pos x1 y1) (Pos x2 y2) = x1*x2+y1*y2

-- Define an equality test for a position
instance Eq Position where
    (==) (Pos x1 y1) (Pos x2 y2) = (x1 == x2) && (y1 == y2)

-- Define the string representation for a position
instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
