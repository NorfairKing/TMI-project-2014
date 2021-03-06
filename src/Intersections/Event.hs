module Intersections.Event where

import Geometry.Circle
import Geometry.Position

data Event = Insert Circle | Delete Circle

instance Eq Event where
    (==) e1 e2 = (==) (pos e1) (pos e2)

instance Ord Event where
    compare = eventOrdering

eventOrdering  e1 e2 = positionOrdering  (pos e1) (pos e2)
eventOrdering' e1 e2 = positionOrdering' (pos e1) (pos e2)

instance Show Event where
    show e = case e of
        Insert c -> "Insert " ++ show c        
        Delete c -> "Delete " ++ show c        



-- A circle has two event points
eventPoints :: Circle -> [Event]
eventPoints c = [Insert c, Delete c]

-- All eventpoints of a list of circles
eventPointss :: [Circle] -> [Event]
eventPointss = concatMap eventPoints

pos :: Event -> Position
pos (Insert (Cir (Pos x y) r)) = Pos (x-r) (y-r)
pos (Delete (Cir (Pos x y) r)) = Pos (x+r) (y+r)


