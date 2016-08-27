{-# LANGUAGE BangPatterns #-}

import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Functor.Identity
import System.Environment (getArgs)

type Weight = Int
type Graph r = Array r DIM2 Weight

-- computeP :: (Monad m, Source r2 e, Target r2 e, Load r1 sh e) => Array r1 sh e -> m (Array r2 sh e)
-- The type is almost the same as  computeS, except that computeP takes place in a monad.
-- It works with any monad, and it doesn’t matter which monad is used because the purpose
-- of the monad is only to ensure that computeP operations are performed in sequence and
-- not nested.
-- Monad用于保证并行化中必要的顺序。

shortestPaths :: Graph U -> Graph U
-- Monad只有保证必要顺序的功能，所以用Identity Monad足够了。
shortestPaths g0 = runIdentity $ go g0 0
  where
    Z :. _ :. n = extent g0

    go !g !k | k == n = return g
             | otherwise =
                  computeP (fromFunction (Z :. n :. n) sp) >>= \g' ->
                  go g' (k + 1)
      where
        sp (Z :. i :. j) = min (g ! (Z :. i :.j)) (g ! (Z :. i :. k) + g ! (Z :. k :. j))

-- Is it works???
-- 表现不是很好，相比于原版，gc跑的时间太长了
shortestPaths' :: Graph U -> Graph U
shortestPaths' g0 = runIdentity $ computeP $ go (d2u g0) 0
  where
    Z :. _ :. n = extent g0

    go g !k | k == n = g
            | otherwise =
              let 
                g' = fromFunction (Z :. n :. n) sp
              in
                go g' (k + 1)
      where 
        sp (Z :. i :. j) = min (g ! (Z :. i :. j)) (g ! (Z :. i :. k) + g ! (Z :. k :. j))

d2u :: Graph U -> Graph D
d2u g = fromFunction (extent g) (\ix -> g ! ix)

-- Testing

input :: [[Int]]
input = [[  0, 999, 999,  13, 999, 999],
         [999,   0, 999, 999,   4,   9],
         [ 11, 999,   0, 999, 999, 999],
         [999,   3, 999,   0, 999,   7],
         [ 15,   5, 999,   1,   0, 999],
         [ 11, 999, 999,  14, 999,   0]]

-- correct result:
result :: [[Int]]
result = [[0,  16, 999, 13, 20, 20],
          [19,  0, 999,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, 999,  0,  7,  7],
          [15,  4, 999,  1,  0,  8],
          [11, 17, 999, 14, 21,  0] ]

test :: Bool
test = fromAdjMatrix (shortestPaths (toAdjMatrix input)) == result

toAdjMatrix :: [[Int]] -> Graph U
toAdjMatrix xs = fromListUnboxed (Z :. k :. k) (concat xs)
  where k = length xs

fromAdjMatrix :: Graph U -> [[Int]]
fromAdjMatrix m = chunk k (toList m)
  where
   (Z :. _ :. k) = extent m

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

main :: IO ()
main = print test
