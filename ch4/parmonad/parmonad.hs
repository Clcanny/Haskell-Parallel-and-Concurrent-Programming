import Control.Monad.Par (Par, runPar, fork, new, get, put)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

parFib :: Par Int
parFib = 
    new >>= \i ->
    new >>= \j ->
    fork (put i (fib 31)) >>
    fork (put j (fib 32)) >>
    get i >>= \a ->
    get j >>= \b ->
    return (a + b)

main = do
    print $ runPar parFib
