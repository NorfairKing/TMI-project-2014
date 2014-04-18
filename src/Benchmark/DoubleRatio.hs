module Benchmark.DoubleRatio where

import Data.List
import Benchmark.Settings
import Benchmark.Benchmark

drCases :: Int -> [[Case]]
drCases na = map (drCases' na) drScs

drCases' :: Int -> Double -> [Case]
drCases' na sc = [ (C na nc sc) | nc <- drNcs]

doubleRatio na = do
    mx <- mapM (mapM benchSolve) $ drCases na
    return $ map (\x -> zipWith (/) (tail x) x) $ transpose mx
