module Helper (
      printTimeSince
    , getCurrentTime
    , fib
    , rpar
    , rseq
    , runEval
    , mainHelper
) where

import Data.Time.Clock
import Text.Printf
import Control.Exception
import Control.Parallel
import Control.Parallel.Strategies

printTimeSince t0 = do
    t1 <- getCurrentTime
    printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mainHelper :: Show a => a -> IO ()
mainHelper f = do
    t0 <- getCurrentTime
    r <- evaluate f
    printTimeSince t0
    print r
    printTimeSince t0
