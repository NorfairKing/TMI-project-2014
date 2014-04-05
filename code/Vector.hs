module Vector where

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
