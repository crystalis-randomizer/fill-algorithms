import System.Environment
import Graph

main = do
  args <- getArgs
  putStrLn "Random Fill"
  putStrLn $ analyzeItems $ randomFill2 $ graph args
  putStrLn "Assumed Fill"
  putStrLn $ analyzeItems $ assumedFill2 $ graph args
  putStrLn "Forward Fill"
  putStrLn $ analyzeItems $ forwardFill2 $ graph args
