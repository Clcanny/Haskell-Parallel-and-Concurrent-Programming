import Helper

g :: (Integer, Integer)
g = runEval $
    rpar (fib 35) >>= \a ->
    rseq (fib 30) >>= \b ->
    return (a, b)

main = mainHelper g
