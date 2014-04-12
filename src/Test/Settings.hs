module Test.Settings where

import Text.Printf

algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [c*10^e | e <- [0..1], c<- [1..9]]

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
