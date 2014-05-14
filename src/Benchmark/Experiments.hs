module Benchmark.Experiments where

import Benchmark.Settings
import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment

allExperiments :: [Experiment]
allExperiments = 
    
    [
     fewIntersectionsExperiment
     , averageCaseExperiment
     , manyIntersectionsExperiment
     , threeDPlotExperiment
     , nrIntersectionsExperiment
    ]
    
    ++

    map doublingRatioExperiment nas
    

accuracy :: Int
accuracy = 10

-- 3D Plot Experiment
threeDPlotExperiment :: Experiment
threeDPlotExperiment = RawDataExperiment "3D"
    [ 
    A accuracy (C na nc sc) 
    | nc <- [0, 10 .. 200]
    , na <- nas
    , sc <- [0.0, 0.05 .. 1]
    ]


-- Few Intersections Experiment
fewIntersectionsExperiment :: Experiment
fewIntersectionsExperiment = RawDataExperiment "FewIntersections"
    [
    A accuracy (C na nc 0.001)
    | nc <- [10, 15 .. 500]
    , na <- nas
    ]

-- Few Intersections Experiment
averageCaseExperiment :: Experiment
averageCaseExperiment = RawDataExperiment "AverageCase"
    [
    A accuracy (C na nc 0.5)
    | nc <- [10, 15 .. 500]
    , na <- nas
    ]

-- Many Intersections Experiment
manyIntersectionsExperiment :: Experiment
manyIntersectionsExperiment = RawDataExperiment "ManyIntersections"
    [
    A accuracy (C na nc 1000)
    | nc <- [10, 15 .. 500]
    , na <- nas
    ]

drAccuracy = 10

drNcs = [ 10 * 2^i | i <- [0 .. 7] ]
drScs = [ 0, 0.001, 0.5, 1 ]

doublingRatioExperiment :: Int -> Experiment
doublingRatioExperiment na = DoublingRatioExperiment ("DoublingRatio_" ++ show na)
    [
    [ A drAccuracy (C na nc sc) | nc <- drNcs ]
    | sc <- drScs
    ]

nrIntersectionsExperiment :: Experiment
nrIntersectionsExperiment = NrIntersectionsExperiment "NrIntersections" 
    [
    (nc, sc) 
    | nc <- [0, 10 .. 200]
    , sc <- [0.0, 0.05 .. 1]
    ]

    

