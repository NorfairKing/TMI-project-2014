module Visual.SVG where

import Geometry.Circle
import Geometry.Position

-- Settings
scale               = 250.0
offset              = 1.0
width               = 1000
height              = 1000
background          = "#000000"
circleColor         = "#ffffff"
intersectionColor   = "#ff0000"

class Drawable a where
    draw :: a -> String

data SVGUtil = Header
                | Background
                | Footer

instance Drawable SVGUtil where
    draw Header
        = "<svg "
            ++ " width=\"" ++ show width ++ "\""
            ++ " height=\"" ++ show height ++ "\""
            ++ " >"
    
    draw Background
        = " <rect "
            ++ " width=\"100%\""
            ++ " height=\"100%\""
            ++ " fill=\"" ++ background ++ "\""
            ++ " />"
    
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
