module Test where

import Control.Monad
import Control.Monad.State
import System.Cmd
import System.IO
import System.Random

import Parser
import Circle
import Position


algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [ c*10^e | e <- [0..1], c<- [1..9] ]


test :: [String] -> IO () 
test args = do
    let [command] = args
    case command of
        "generate"  -> generateTests
        "run"       -> runTests
        "compare"   -> compareTests
        "all"       -> do
                generateTests
                runTests
                compareTests


-- Helper function
doAll :: [IO ()] -> IO ()
doAll fs = foldr (>>) (return ()) fs


-- Generation
generateTests :: IO ()
generateTests = doAll $ map generateTestCase circlesNrs

generateTestCase :: Int -> IO ()
generateTestCase nc = do
    circles <- replicateM nc getRandomCircle
    mapM_ (makeFile circles) algorithmNrs

makeFile :: [Circle] -> Int -> IO ()
makeFile cs na = do
    outh <- openFile ("test_input/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt") WriteMode
    hPutStrLn outh $ show na
    hPutStrLn outh $ show nc
    mapM_ (hPutStrLn outh . show) cs
    hClose outh
    putStrLn $ show na ++ "-" ++ show nc ++ " generated."
    where nc = length cs


getRandomCircle :: IO Circle
getRandomCircle = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (Cir (Pos x y) r)


-- Running
runTests :: IO ()
runTests = doAll $ map runTest circlesNrs

runTest :: Int -> IO ()
runTest nc = mapM_ (runTestIndividual nc) algorithmNrs

runTestIndividual nc na = do
    system (cmd na nc)
    putStrLn $ show na ++ "-" ++ show nc ++ " done."

cmd na nc = "./Main" ++ " " ++ input ++ " " ++ output
    where
        input = "< test_input/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt"
        output = "> test_output/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt"


-- Comparison
compareTests :: IO ()
compareTests = doAll $ map compareTest circlesNrs

compareTest :: Int -> IO ()
compareTest nc = do
    expected:results <- mapM (getResults nc) algorithmNrs
    let diffs = zip [1..] $ map (countDifferences expected) results
    mapM_ putOutcome diffs
    where
        putOutcome (na , (correct, total))
            = if correct == total
              then putStrLn $ caseStr ++ " SUCCESS " ++ ratStr
              else putStrLn $ caseStr ++ " FAILURE " ++ ratStr
            where
                ratStr = show correct ++ "/" ++ show total
                caseStr = show na ++ "-" ++ show nc

getResults :: Int -> Int -> IO [Position]
getResults nc na = do
    inh <- openFile ("test_output/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt") ReadMode
    ll <- (drop 2 . reverse . lines) `fmap` hGetContents inh
    let np = length ll
    let (positions, _) = runState (replicateM np parseLine) ll
    return positions

-- Count the difference between two lists, arranged in any order.
countDifferences :: Eq a => [a] -> [a] -> (Int, Int)
countDifferences rs ss = (correct, total)
    where
        correct = sum $ map fromEnum comb
        total = length comb
        comb = sInR ++ rInS
        rInS = map (`elem` ss) rs
        sInR = map (`elem` rs) ss    








