module ParList (
      parMap
    , parMap'
) where

import Control.Parallel
import Control.Parallel.Strategies (Eval, rparWith, rseq, using)
import Control.DeepSeq (NFData, force)

-- rdeepseq :: NFData a => Strategy a
rdeepseq :: NFData a => a -> Eval a
rdeepseq = rseq . force

-- evalList :: Strategy a -> Strategy [a]
evalList :: (a -> Eval a) -> [a] -> Eval [a]
evalList strat [] = return []
evalList strat (x : xs) =
    strat x >>= \x' ->
    evalList strat xs >>= \xs' ->
    return (x' : xs')

-- parList :: Strategy a -> Strategy [a]
parList :: (a -> Eval a) -> [a] -> Eval [a]
parList strat = evalList (rparWith strat)

parMap :: NFData b => (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rdeepseq

parMap' :: (a -> b) -> [a] -> [b]
parMap' f xs = map f xs `using` parList rseq
