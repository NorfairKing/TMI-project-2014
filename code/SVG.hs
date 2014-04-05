module SVG where

import Circle
import Position

class Drawable a where
    draw :: a -> String

scale = 1000.0

-- Define a circle to be drawable
instance SVG.Drawable Circle where
    draw (Cir (Pos x y) r)
        =   "    <circle"
         ++ " cx=\"" ++ show x' ++ "\""
         ++ " cy=\"" ++ show y' ++ "\""
         ++ " r=\""  ++ show r' ++ "\""
         ++ " stroke=\"white\""
         ++ " stroke-width=\"1\""
         ++ " style=\"fill-opacity:0.0;stroke-opacity:1\""
         ++ " />"
        where
            [x',y',r'] = map (floor.(*SVG.scale)) [x,y,r]

instance SVG.Drawable Position where
    draw (Pos x y) = "    <circle"
                        ++ " cx=\"" ++ show x' ++ "\""
                        ++ " cy=\"" ++ show y' ++ "\""
                        ++ " r=\""  ++ show r  ++ "\""
                        ++ " fill=\"red\""
                        ++ " />"
        where
            [x',y'] = map (floor.(*SVG.scale)) [x,y]
            r = 3
