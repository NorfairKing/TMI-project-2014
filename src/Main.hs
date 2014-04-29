module Main where

import Benchmark.Benchmark
import Intersections.Intersections
import System.Environment
import Test.Test
import Visual.Visual

dispatch :: String -> Maybe ([String] -> IO ())
dispatch str = lookup str  
            [
              ("test", test)
            , ("visualize", visualize)
            , ("benchmark", const benchmark)
            ]  


main :: IO ()
main = do
    -- Get the command line arguments.
    args <- getArgs  

    case args of
        -- If there are no arguments, run the program normally.
        []                  -> intersections

        -- If there are arguments, the first one represents the command,
        -- while the other ones represent arguments to the command.
        (command:arguments) -> do
                -- Dispatch the command
                let action = dispatch command
                case action of
                    -- If the command is not in the dispatch list, do nothing.
                    Nothing     -> putStrLn "Unknown command."
                    
                    -- Otherwise, run the appropriate function.
                    Just act    -> act arguments
