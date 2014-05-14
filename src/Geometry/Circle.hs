module Geometry.Circle where

import Data.COrdering
import Control.DeepSeq
import Text.Printf

import Geometry.Position

data Circle = Cir { centre :: Position
                  , radius :: Double
                  }

-- Define a string representation for circles
instance Show Circle where
    show (Cir (Pos x y) r) = unwords $ map (printf "%0.15f") [x,y,r]

-- Define equality for circles
instance Eq Circle where
    (==) (Cir (Pos x1 y1) r1) (Cir (Pos x2 y2) r2)
        = abs(x1-x2) < precision
        && abs(y1-y2) < precision
        && abs(r1-r2) < precision
        where precision = 10**(-12)


-- Test whether two circles intersect
circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect (Cir p1 r1) (Cir p2 r2)
    =  d <= r1 + r2 
    && d >= abs (r1 - r2)
    where d = distance p1 p2
    
circlesNrIntersections :: Circle -> Circle -> Int
circlesNrIntersections c1@(Cir p1 r1) c2@(Cir p2 r2) = 
    if circlesIntersect c1 c2
    then 
        if r1 + r2 == d
        then 1
        else 2
    else 0
    where d = distance p1 p2

totalNrIntersections :: [Circle] -> Int
totalNrIntersections [] = 0
totalNrIntersections [_] = 0
totalNrIntersections (c:cs)
    = sum (map (circlesNrIntersections c) cs)
    + totalNrIntersections cs

-- Give the intersection points of two circles.
-- This works for two distinct circles, with two different intersections
-- This works in O(1) time
circlesIntersections :: Circle -> Circle -> [Position]
circlesIntersections c1@(Cir p1@(Pos x1 y1) r1) c2@(Cir p2@(Pos x2 y2) r2) =
    --if circlesIntersect c1 c2 && (c1 /= c2) 
    --    then [ Pos px1 py1, Pos px2 py2 ]
    --    else []
    [ Pos px1 py1, Pos px2 py2 ]
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
        d   = distance p1 p2

instance NFData Circle

-- Circle orderings
xOrdering, yOrdering, rOrdering :: Circle -> Circle -> Ordering
xOrdering (Cir (Pos x1 _ ) _) (Cir (Pos x2 _ ) _) = compare x1 x2
yOrdering (Cir (Pos _  y1) _) (Cir (Pos _  y2) _) = compare y1 y2
rOrdering (Cir _ r1) (Cir _ r2) = compare r1 r2

-- Circle COrderings
xCOrdering, yCOrdering, rCOrdering :: Circle -> Circle -> COrdering Circle
xCOrdering = co xOrdering
yCOrdering = co yOrdering
rCOrdering = co rOrdering

-- Helper
co :: (a -> b -> Ordering) -> a -> b -> COrdering a
co = fstByCC

-- Quick overlap functions
xOverlap, yOverlap :: Circle -> Circle -> Bool
xOverlap (Cir (Pos x1 _) r1) (Cir (Pos x2 _ ) r2) = abs(r2+r1) > abs(x2-x1)
yOverlap (Cir (Pos _ y1) r1) (Cir (Pos _  y2) r2) = abs(r2+r1) > abs(y2-y1)

