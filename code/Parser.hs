module Parser where

import Control.Monad.State

import Position
import Circle

type Loader a = State [String] a

class Parse a where
    parse :: String -> a

instance Parse Char where
    parse [x] = x

instance Parse Int where
    parse = read

instance Parse Double where
    parse = read

instance Parse a => Parse [a] where
    parse = map parse . words

instance (Parse a, Parse b) => Parse (a, b) where
    parse str = (parse x, parse y)
        where [x, y] = words str

instance (Parse a, Parse b, Parse c) => Parse (a, b, c) where
    parse str = (parse x, parse y, parse z)
        where [x, y, z] = words str

instance Parse Position where
    parse str = Pos x y
        where [x, y] = (map read . words) str
    
instance Parse Circle where
    parse str = Cir (Pos x y) z
        where [x, y, z] = (map read . words) str


nextLine :: Loader String
nextLine = do (line:lines) <- get
              put lines
              return line

parseLine :: Parse a => Loader a
parseLine = do line <- nextLine
               return $ parse line
