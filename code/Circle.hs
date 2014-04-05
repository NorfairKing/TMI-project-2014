module Circle where

import Text.Printf

import SVG

data Circle = Cir Double Double Double

-- Define a string representation for circles
instance Show Circle where
    show (Cir x y r) = unwords $ map (printf "%0.15f") [x,y,r]

-- Define equality for circles
instance Eq Circle where
    (==) (Cir x1 y1 r1) (Cir x2 y2 r2) = abs(x1-x2) < precision
                                         && abs(y1-y2) < precision
                                         && abs(r1-r2) < precision
        where precision = 10**(-12)

-- Define a circle to be drawable
instance SVG.Drawable Circle where
    draw (Cir x y r) = "    <circle"
                        ++ " cx=\"" ++ show x' ++ "\""
                        ++ " cy=\"" ++ show y' ++ "\""
                        ++ " r=\""  ++ show r' ++ "\""
                        ++ " stroke=\"white\""
                        ++ " stroke-width=\"1\""
                        ++ " style=\"fill-opacity:0.0;stroke-opacity:1\""
                        ++ " />"
        where
            [x',y',r'] = map (floor.(*SVG.scale)) [x,y,r]
