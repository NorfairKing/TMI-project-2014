module Benchmark.Assignment where

import Benchmark.Case

data Assignment = A Int Case

instance Show Assignment where
    show (A nt c) = "A " ++ show nt ++ " " ++ show c
