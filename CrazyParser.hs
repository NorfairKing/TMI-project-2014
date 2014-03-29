module CrazyParser where

import Control.Monad.State

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
  parse str = let [x, y] = words str
              in
               (parse x, parse y)

instance (Parse a, Parse b, Parse c) => Parse (a, b, c) where
  parse str = let [x, y, z] = words str
              in
               (parse x, parse y, parse z)


nextLine :: Loader String
nextLine = do (line:lines) <- get
              put lines
              return line

parseLine :: Parse a => Loader a
parseLine = do line <- nextLine
               return $ parse line
