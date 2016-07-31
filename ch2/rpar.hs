import Control.Parallel.Strategies

f :: Int -> Int
f 0 = 1
f 1 = 1
f n = f (n -  1) + f (n - 2)

g = runEval $
    rpar (f 20) >>= \a ->
    rpar (f 21) >>= \b ->
    rseq a >>
    rseq b >>
    return (a, b)

main = return g
