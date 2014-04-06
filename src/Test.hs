module Test where

import Control.Monad
import Control.Monad.State
import System.Cmd
import System.IO
import System.Random

import Circle
import Parser
import Position

-- Settings
algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [c*10^e | e <- [0..1], c<- [1..9]]

inputDir       = "test_input"
outputDir      = "test_output"
casePrefix     = "testcase"
caseExtension  = ".txt"

caseName na nc
    =  casePrefix
    ++ show na
    ++ "_"
    ++ show nc
    ++ caseExtension

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


-- Generation
generateTests :: IO ()
generateTests = mapM_ generateTestCase circlesNrs

generateTestCase :: Int -> IO ()
generateTestCase nc = do
    circles <- replicateM nc getRandomCircle
    mapM_ (makeFile circles) algorithmNrs

makeFile :: [Circle] -> Int -> IO ()
makeFile cs na = do
    outh <- openFile (
                inputDir 
                ++ "/" 
                ++ caseName na nc
                ) WriteMode
    hPrint outh na
    hPrint outh nc
    mapM_ (hPrint outh) cs
    hClose outh
    putStrLn $ caseName na nc ++ " generated."
    where nc = length cs


getRandomCircle :: IO Circle
getRandomCircle = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double

    r <- randomIO :: IO Double
    return (Cir (Pos x y) r)


-- Running
runTests :: IO ()
runTests = mapM_ runTest circlesNrs

runTest :: Int -> IO ()
runTest nc = mapM_ (runTestIndividual nc) algorithmNrs

runTestIndividual nc na = do
    system (cmd na nc)
    putStrLn $ caseName na nc ++ " done."

cmd na nc = "./Main" ++ " " ++ input ++ " " ++ output
    where
        input = "< " 
            ++ inputDir 
            ++ "/" 
            ++ caseName na nc
        output = "> " 
            ++ outputDir 
            ++ "/" 
            ++ caseName na nc


-- Comparison
compareTests :: IO ()
compareTests = mapM_ compareTest circlesNrs

compareTest :: Int -> IO ()
compareTest nc = do
    expected:results <- mapM (getResults nc) algorithmNrs
    let diffs = zip [2..] $ map (countDifferences expected) results
    mapM_ putOutcome diffs
    where
        putOutcome (na , (correct, total))
            = if correct == total
              then putStrLn $ caseStr ++ " SUCCESS " ++ ratStr
              else putStrLn $ caseStr ++ " FAILURE " ++ ratStr
            where
                ratStr = show correct ++ "/" ++ show total
                caseStr = caseName na nc

getResults :: Int -> Int -> IO [Position]
getResults nc na = do
    inh <- openFile (
            outputDir 
            ++ "/" 
            ++ casePrefix 
            ++ "_" 
            ++ show na 
            ++ "_" 
            ++ show nc 
            ++ caseExtension
            ) ReadMode
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
