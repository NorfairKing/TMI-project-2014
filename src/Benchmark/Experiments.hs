module Benchmark.Experiments where

import Benchmark.Settings
import Benchmark.Assignment
import Benchmark.Case
import Benchmark.Experiment

allExperiments = 
    [
      fewIntersectionsExperiment
    , manyIntersectionsExperiment
    , threeDPlotExperiment
    ]


accuracy = 5


-- 3D Plot Experiment
threeDPlotExperiment = E "3D"
    [ 
    A accuracy (C na nc sc) 
    | nc <- [0,5..100]
    , na <- nas
    , sc <- [0.0,0.05..0.5]
    ]


-- Few Intersections Experiment
fewIntersectionsExperiment = E "FewIntersections"
    [
    A accuracy (C na nc 0.002)
    | nc <- [10..1000]
    , na <- nas
    ]

-- Many Intersections Experiment
manyIntersectionsExperiment = E "ManyIntersections"
    [
    A accuracy (C na nc 0.4)
    | nc <- [10..100]
    , na <- nas
    ]
