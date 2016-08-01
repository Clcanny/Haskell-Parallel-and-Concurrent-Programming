import Helper

parPair :: Strategy (a, b)
parPair (a, b) = 
    rpar a >>= \a' ->
    rpar b >>= \b' ->
    return (a, b)

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = 
    sa a >>= \a' ->
    sb b >>= \b' ->
    return (a', b')

parPair' :: Strategy a -> Strategy b -> Strategy (a, b)
parPair' sa sb = evalPair (rparWith sa) (rparWith sb)

rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

-- main = mainHelper ((fib 35, fib 36) `using` parPair)
-- main = mainHelper (runEval $ parPair (fib 40, fib 1))
main = mainHelper ((fib 35, fib 36) `using` (parPair' rdeepseq rdeepseq))
