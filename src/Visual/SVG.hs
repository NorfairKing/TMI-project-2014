module Visual.SVG where

import Geometry.Circle
import Geometry.Position

-- Settings
scale               = 200.0
offset              = 1.0
width               = 600
height              = 600
circleColor         = "#000000"
intersectionColor   = "#ff0000"

class Drawable a where
    draw :: a -> String

data SVGUtil = Header
                | Footer

instance Drawable SVGUtil where
    draw Header
        = "<svg "
            ++ " width=\"" ++ show width ++ "\""
            ++ " height=\"" ++ show height ++ "\""
            ++ " >"
    
    draw Footer
        = "</svg>"



-- Define a circle to be drawable
instance Drawable Circle where
    draw (Cir (Pos x y) r)
        = " <circle"
            ++ " cx=\"" ++ show x' ++ "\""
            ++ " cy=\"" ++ show y' ++ "\""
            ++ " r=\"" ++ show r' ++ "\""
            ++ " stroke=\"" ++ circleColor ++ "\""
            ++ " stroke-width=\"1\""
            ++ " style=\"fill-opacity:0.0;stroke-opacity:1\""
            ++ " />"
        where
            x' = floor $ (x+offset)*scale
            y' = floor $ (y+offset)*scale
            r' = floor $ r*scale

instance Drawable Position where
    draw (Pos x y)
        = " <circle"
            ++ " cx=\"" ++ show x' ++ "\""
            ++ " cy=\"" ++ show y' ++ "\""
            ++ " r=\"" ++ show r ++ "\""
            ++ " fill=\"" ++ intersectionColor ++ "\""
            ++ " />"
        where
            x' = floor $ (x+offset)*scale
            y' = floor $ (y+offset)*scale
            r = 2
