module Benchmark.Experiments where

import Benchmark.Settings
import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment

allExperiments :: [Experiment]
allExperiments = 
    [
      fewIntersectionsExperiment
    , manyIntersectionsExperiment
    , threeDPlotExperiment
    ]


accuracy :: Int
accuracy = 1

-- 3D Plot Experiment
threeDPlotExperiment :: Experiment
threeDPlotExperiment = E "3D"
    [ 
    A accuracy (C na nc sc) 
    | nc <- [0,2..100]
    , na <- nas
    , sc <- [0.0,0.02..0.5]
    ]


-- Few Intersections Experiment
fewIntersectionsExperiment :: Experiment
fewIntersectionsExperiment = E "FewIntersections"
    [
    A accuracy (C na nc 0.002)
    | nc <- [10..1000]
    , na <- nas
    ]

-- Many Intersections Experiment
manyIntersectionsExperiment :: Experiment
manyIntersectionsExperiment = E "ManyIntersections"
    [
    A accuracy (C na nc 0.4)
    | nc <- [10..100]
    , na <- nas
    ]
