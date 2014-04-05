module VectorSpace where 

-- Define a 2D position.
data Position = Pos Scalar Scalar
type Scalar     = Double

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
    -- Magnitude of cross product
    x :: a -> a -> Double
    -- Norm
    norm :: a -> Double
    norm p = sqrt $ p `o` p

-- Declare that positions are vectors with doubles as scalar.
instance Vector Position where
    -- Addition
    (<+>) (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)
    -- Substraction 
    (<->) (Pos x1 y1) (Pos x2 y2) = Pos (x1-x2) (y1-y2)
    -- Scalar multiplication
    (<*>) s (Pos x y) = Pos (s*x) (s*y)
    -- Dotproduct
    o (Pos x1 y1) (Pos x2 y2) = x1*x2+y1*y2
    -- Magnitude of cross product
    x (Pos x1 y1) (Pos x2 y2) = x1*y2 - y1*x2

-- Define an equality test for a position.
instance Eq Position where
    (==) (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) < precision    
                                && abs (y1 - y2) < precision
        where precision = 10**(-12)

-- Define the string representation for a position.
instance Show Position where
    show (Pos x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Ord Position where
    compare (Pos x1 y1) (Pos x2 y2) =
        if c1 == EQ
        then compare y1 y2
        else c1
        where c1 = compare x1 x2

-- Define the euclidean distance between two points.
distance :: Position -> Position -> Double
distance p1 p2 = norm $ p1 <-> p2
