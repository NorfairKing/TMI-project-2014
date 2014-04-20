module Benchmark.Experiment where

import Benchmark.Assignment

data Experiment = E String [Assignment]

instance Show Experiment where
    show (E name as) = "E: " ++ name ++ show as
