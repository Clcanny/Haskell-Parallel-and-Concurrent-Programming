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

-- parList :: Strategy a -> Strategy [a]
parList strat xs = do
        go xs
        return xs
    where
        go [] = return ()
        go (x:xs) = do rparWith strat x
                       go xs

parMap :: NFData b => (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rdeepseq

parMap' :: (a -> b) -> [a] -> [b]
parMap' f xs = map f xs `using` parList rseq
