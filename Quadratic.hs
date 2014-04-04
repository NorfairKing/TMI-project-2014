module Quadratic where

import VectorSpace
import Geometry
import Naive

import Data.List

data Event = Insert Circle | Delete Circle

intersections :: [Circle] -> [Position]
intersections cs = nub $ go (sort $ eventPointss cs) []
    where
        go [] act = Naive.intersections act
        go (e:evl) act = ics ++ go evl act'
            where
                act' = act ++ case e of
                    Insert c -> (c:act)
                    Delete c -> (delete c act)
                ics = Naive.intersections act'

eventPointss = concatMap eventPoints

eventPoints :: Circle -> [Event]
eventPoints c = [Insert c, Delete c]

instance Eq Event where
    (==) e1 e2 = (==) (pos e1) (pos e2)

instance Ord Event where
    compare e1 e2 = compare (pos e1) (pos e2)

instance Show Event where
    show e = case e of
        Insert c -> "Insert " ++ show c        
        Delete c -> "Delete " ++ show c        

pos event = case event of
     Insert (Pos x y, r) -> Pos (x-r) y
     Delete (Pos x y, r) -> Pos (x+r) y
