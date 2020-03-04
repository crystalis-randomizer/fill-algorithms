import System.Environment
import Graph
import Prob

main = do
  args <- getArgs
  let g = graph args
      r = randomFill2 g
      a = assumedFill2 g
      f = forwardFill2 g
  putStr $ unlines ["Random Fill: " ++ show (entropy r)
                   ,analyzeItems r
                   ,"Assumed Fill: " ++ show (entropy a)
                   ,analyzeItems a
                   ,"Forward Fill: " ++ show (entropy f)
                   ,analyzeItems f
                   ]
