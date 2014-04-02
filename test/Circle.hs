import System.IO
import System.Environment

import Data.List

precision = 10**(-12)

data Result = Res Double Double

instance Show Result where
    show (Res x y) = "Res " ++ show x ++ show y

instance Eq Result where
    (==) (Res x1 y1) (Res x2 y2) = abs(x1-x2) < precision
                                   && abs(y1-y2) < precision

data Circle = Cir Double Double Double

instance Show Circle where
    show (Cir x y r) = "Cir " ++ show x ++ show y ++ show z

instance Eq Circle where
    (==) (Cir x1 y1 r1) (Cir x2 y2 r2) = abs(x1-x2) < precision
                                         && abs(y1-y2) < precision
                                         && abs(r1-r2) < precision
