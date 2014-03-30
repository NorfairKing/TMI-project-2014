import Control.Monad
import System.Random
import Text.Printf

main :: IO ()
main = do
    na <- intPrompt "Enter algorithm nr to generate for:"
    nc <- intPrompt "Enter amount of circles to generate:"
    triples <- replicateM nc getRandomTriple
    print na
    print nc  
    mapM_ printTriple triples    

printTriple :: PrintfArg a => (a, a, a) -> IO ()
printTriple (x,y,r) = putStrLn $ unwords $ map (printf "%0.15f") [x,y,r]

getRandomTriple :: IO (Double, Double, Double)
getRandomTriple = do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    r <- randomIO :: IO Double
    return (x,y,r)

intPrompt :: String -> IO Int
intPrompt str = do
    putStr $ str ++ "\n> "
    line <- getLine
    return (read line :: Int)

