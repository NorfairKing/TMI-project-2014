module Benchmark.Experiment where

import Benchmark.Assignment

data Experiment 
    = RawDataExperiment String [Assignment]
    | DoublingRatioExperiment String [[Assignment]]


instance Show Experiment where
    show (RawDataExperiment name as) = "RawDataExperiment: " ++ name ++ show as
    show (DoublingRatioExperiment name ass) = "DoublingRatioExperiment: " ++ name ++ show ass
