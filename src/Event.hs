module Event where

import Circle
import Position


data Event = Insert Circle | Delete Circle

instance Eq Event where
    (==) e1 e2 = (==) (pos e1) (pos e2)

instance Ord Event where
    compare e1 e2 = compare (pos e1) (pos e2)

instance Show Event where
    show e = case e of
        Insert c -> "Insert " ++ show c        
        Delete c -> "Delete " ++ show c        

eventPointss :: [Circle] -> [Event]
eventPointss = concatMap eventPoints

eventPoints :: Circle -> [Event]
eventPoints c = [Insert c, Delete c]

pos :: Event -> Position
pos (Insert (Cir (Pos x y) r)) = Pos (x-r) y
pos (Delete (Cir (Pos x y) r)) = Pos (x+r) y

