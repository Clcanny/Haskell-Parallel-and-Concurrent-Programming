module Rpar3 where

import Helper

g :: (Integer, Integer)
g = runEval $
    rpar (fib 35) >>= \a ->
    rpar (fib 30) >>= \b ->
    rseq a >>
    rseq b >>
    return (a, b)

main = mainHelper g
