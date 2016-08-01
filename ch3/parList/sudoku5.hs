import Sudoku
import ParList
import Control.Exception
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
-- <<solutions
  -- let solutions = map solve puzzles `using` parList rseq
  let solutions = parMap solve puzzles
-- >>

  print (length (filter isJust solutions))
