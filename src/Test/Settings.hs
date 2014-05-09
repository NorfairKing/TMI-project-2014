module Test.Settings where

import Text.Printf

algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [0,1,10]

inputDir       = "test_input"
outputDir      = "test_output"
casePrefix     = "testcase"
caseExtension  = ".txt"

caseName :: Int -> Int -> String
caseName na nc
    =  casePrefix
    ++ "_"
    ++ show na
    ++ "_"
    ++ printf "%03d" nc
    ++ caseExtension
