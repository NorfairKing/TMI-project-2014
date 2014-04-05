module Quadratic where

import Data.List

import Circle
import Position

import Naive

intersections :: [Circle] -> [Position]
intersections cs = nub $ go (sort $ eventPointss cs) []
    where
        go [] act = Naive.intersections act
        go (Insert c : evl) act = concatMap (circlesIntersections c) act ++ go evl (c:act)
        go (Delete c : evl) act = go evl (delete c act)

eventPointss :: [Circle] -> [Event]
eventPointss = concatMap eventPoints

eventPoints :: Circle -> [Event]
eventPoints c = [Insert c, Delete c]

data Event = Insert Circle | Delete Circle

instance Eq Event where
    (==) e1 e2 = (==) (pos e1) (pos e2)

instance Ord Event where
    compare e1 e2 = compare (pos e1) (pos e2)

instance Show Event where
    show e = case e of
        Insert c -> "Insert " ++ show c        
        Delete c -> "Delete " ++ show c        

pos :: Event -> Position
pos event = case event of
     Insert (Cir (Pos x y) r) -> Pos (x-r) y
     Delete (Cir (Pos x y) r) -> Pos (x+r) y
