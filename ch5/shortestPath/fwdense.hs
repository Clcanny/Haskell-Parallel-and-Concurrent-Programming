{-# LANGUAGE BangPatterns #-}

import Data.Array.Repa
import qualified Data.Array.Repa as R

type Weight = Int
type Graph r = Array r DIM2 Weight

shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0
  where
    -- using pattern matching to get n
    Z :. _ :. n = extent g0

    -- end if k > n - 1, where (n - 1) is the max vertex
    go !g !k | k == n = g
             | otherwise =
                let 
                  g' = computeS (fromFunction (Z :. n :. n) sp)
                in 
                  -- from unbox array to unbox array
                  -- prdouce many intermediate arrays
                  go g' (k + 1)
      where
        sp (Z :. i :. j) = min (g ! (Z :. i :.j)) (g ! (Z :. i :. k) + g ! (Z :. k :. j))

shortestPaths' :: Graph U -> Graph U
-- d2u g0应该不会有很大的性能开销
-- 因为最终只是去根据索引取得元素而已，没有其他的操作（例如复制数组）
shortestPaths' g0 = computeS $ go (d2u g0) 0
  where
    Z :. _ :. n = extent g0

    -- from delay array to delay array
    -- no intermediate arrays
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
