module Run where

import System.Cmd

import TestCases

runAll :: IO ()
runAll = do
    foldr (>>) (return ()) [ runTest na nc | na <- algorithmNrs, nc <- circlesNrs ]


runTest na nc = do
    system (cmd na nc)
    putStrLn $ show na ++ "-" ++ show nc ++ " done."

cmd na nc = "../Main" ++ " " ++ input ++ " " ++ output
    where
        input = "< testInput/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt"
        output = "> testResults/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt"
