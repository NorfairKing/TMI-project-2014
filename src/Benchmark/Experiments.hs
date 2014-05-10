module Benchmark.Experiments where

import Benchmark.Settings
import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment

allExperiments :: [Experiment]
allExperiments = 
    [
    --  fewIntersectionsExperiment
    --, manyIntersectionsExperiment
    --, threeDPlotExperiment
    doublingRatioExperiment 1
    ]


accuracy :: Int
accuracy = 10

-- 3D Plot Experiment
threeDPlotExperiment :: Experiment
threeDPlotExperiment = RawDataExperiment "3D"
    [ 
    A accuracy (C na nc sc) 
    | nc <- [0,1..100]
    , na <- nas
    , sc <- [0.0,0.01..0.5]
    ]


-- Few Intersections Experiment
fewIntersectionsExperiment :: Experiment
fewIntersectionsExperiment = RawDataExperiment "FewIntersections"
    [
    A accuracy (C na nc 0.002)
    | nc <- [10..1000]
    , na <- nas
    ]

-- Many Intersections Experiment
manyIntersectionsExperiment :: Experiment
manyIntersectionsExperiment = RawDataExperiment "ManyIntersections"
    [
    A accuracy (C na nc 1000)
    | nc <- [10..1000]
    , na <- nas
    ]

drAccuracy = 10

doublingRatioExperiment :: Int -> Experiment
doublingRatioExperiment na = DoublingRatioExperiment ("DoublingRatio_" ++ show na)
    [
    [ A drAccuracy (C na nc sc) | nc <- [ 10 * 2^i | i <- [0..5] ] ]
    | sc <- [ 0.1, 0.2 .. 0.5 ]
    ]



    

