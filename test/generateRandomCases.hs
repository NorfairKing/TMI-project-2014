import Control.Monad
import System.Random
import Text.Printf
import System.IO

main :: IO ()
main = do
    foldr (>>) (return ()) [ generateTestCase na nc | na <- nas, nc <- ncs ]
    where
        nas = [1..3]
        ncs = [ c*10^e | e <- [1..3], c<- [1..10] ]       


getRandomTriple :: IO (Double, Double, Double)
getRandomTriple = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (x,y,r)

intPrompt :: String -> IO Int
intPrompt str = do
    putStrLn str 
    putStr "> "
    line <- getLine
    return (read line :: Int)

generateTestCase na nc = do
    outh <- openFile ("testcases/testcase_" ++ show na ++ "_" ++ show nc ++ ".txt") WriteMode
    print na
    print nc
    triples <- replicateM nc getRandomTriple
    let str = unwords 
    mapM_ (putTriple outh) triples
    hClose outh
    where putTriple outh (x,y,r) = hPutStrLn outh $ unwords $ map (printf "%0.15f") [x,y,r]
