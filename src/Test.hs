module Test where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Cmd
import System.Directory
import System.IO
import System.Random
import Text.Printf

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

caseName :: Int -> Int -> String
caseName na nc
    =  casePrefix
    ++ "_"
    ++ show na
    ++ "_"
    ++ printf "%03d" nc
    ++ caseExtension

test :: [String] -> IO () 
test args = do
    check
    let [command] = args
    case command of
        "generate"  -> generateTests
        "run"       -> runTests
        "compare"   -> compareTests
        "all"       -> do
                generateTests
                runTests
                compareTests

check = do
    bs <- mapM doesDirectoryExist dirs
    mapM_ createDirectory $ select (map not bs) dirs
    where
        dirs = [inputDir, outputDir]

select :: [Bool] -> [a] -> [a]
select [] [] = []
select  _ [] = []
select [] _  = []
select (b:bs) (a:as)
    = if b
    then a : select bs as
    else select bs as

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
runTests = do
    files <- (drop 2 . sort) `fmap` getDirectoryContents inputDir
    mapM_ runTest files

runTest file = do
    system $ cmd file 
    putStrLn $ file ++ " done."

cmd file = "./Main" ++ " " ++ input ++ " " ++ output
    where
        input = "< " 
            ++ inputDir 
            ++ "/" 
            ++ file
        output = "> " 
            ++ outputDir 
            ++ "/" 
            ++ file


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
            = putStrLn $
                if correct == total
                then caseStr ++ " SUCCESS " ++ ratStr
                else caseStr ++ " FAILURE " ++ ratStr
            where
                ratStr = show correct ++ "/" ++ show total
                caseStr = caseName na nc

getResults :: Int -> Int -> IO [Position]
getResults nc na = do
    inh <- openFile (
            outputDir 
            ++ "/" 
            ++ caseName na nc
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
