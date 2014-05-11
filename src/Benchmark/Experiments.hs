module Benchmark.Experiments where

import Benchmark.Settings
import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment

allExperiments :: [Experiment]
allExperiments = 
    [
--      fewIntersectionsExperiment
     manyIntersectionsExperiment
--    , threeDPlotExperiment
    ]
--    ++
--    map doublingRatioExperiment nas
    


accuracy :: Int
accuracy = 10

-- 3D Plot Experiment
threeDPlotExperiment :: Experiment
threeDPlotExperiment = RawDataExperiment "3D"
    [ 
    A accuracy (C na nc sc) 
    | nc <- [0,2..200]
    , na <- nas
    , sc <- [0.0,0.02..1]
    ]


-- Few Intersections Experiment
fewIntersectionsExperiment :: Experiment
fewIntersectionsExperiment = RawDataExperiment "FewIntersections"
    [
    A accuracy (C na nc 0.002)
    | nc <- [10..500]
    , na <- nas
    ]

-- Many Intersections Experiment
manyIntersectionsExperiment :: Experiment
manyIntersectionsExperiment = RawDataExperiment "ManyIntersections"
    [
    A accuracy (C na nc 1000)
    | nc <- [10..500]
    , na <- nas
    ]

drAccuracy = 10

drNcs = [ 5 * 2^i | i <- [0..8] ]
drScs = [ 0.001 * 2^i | i <- [0..8]  ]

doublingRatioExperiment :: Int -> Experiment
doublingRatioExperiment na = DoublingRatioExperiment ("DoublingRatio_" ++ show na)
    [
    [ A drAccuracy (C na nc sc) | nc <- drNcs ]
    | sc <- drScs
    ]



    

