{-# LANGUAGE TypeOperators #-}

import Data.Array.Repa

-- Array :: * -> * -> *
-- data Array r sh e
-- The r parameter is the representation type, which describes how the array is
-- represented in memory, such as U and D.
-- The sh parameter describes the shape of the array; that is, the number of
-- dimensions it has.
-- The e parameter is the type of the elements, for example Double, Int, or Word8.

-- Shapes are built out of two type constructors, Z and (:.):
-- data Z = Z
-- data tail :. head = tail :. head
-- confusing things:
-- (:.) :: tail -> head -> tail :. head (value constructor)
-- (:.) :: * -> * -> * (type constructor)
-- Z :: Z (value constructor)
-- Z :: * (type constructor)
dim :: (Z :. Int) :.Int
dim = Z :. 3 :. 3
-- When using Z :. 3, it means either "shapes" or "indices".
-- the shape of three-element vectors, or the index of the fourth element of a
-- vector （大白话就是既是数组容量又是数组索引）

-- type DIM0 = Z
-- type DIM1 = DIM0 :. Int
-- type DIM2 = DIM1 :. Int

-- fromListUnboxed :: (Shape sh, Unbox a) => sh -> [a] -> Array U sh a
arr :: Array U DIM1 Int
arr = fromListUnboxed (Z :. 6) [1, 2, 3, 4, 5, 6]

-- (!) :: (Shape sh, Source r e) => Array r sh e -> sh -> e
ele = arr ! (Z :. 3)

-- toIndex :: Shape sh => sh -> sh -> Int
-- convert an index to an Int offset, given a shape
offset :: Int
offset = toIndex (Z :. 5 :. 4 :: DIM2) (Z :. 2 :. 2)
-- offset = 2 * 4 + 2 = 10

-- Because the layout of an array in memory is the same regardless of tis shape,
-- we can even change the shape without copying the array:
arr' :: Array U DIM2 Int
arr' = fromListUnboxed (Z :. 3 :. 4) [1, 2 .. 12]

-- from two dimensional array to two dimensional array
ele' = reshape (Z :. 2 :. 6 :: DIM2) arr' ! (Z :. 1 :. 3)
-- from one dimensional array to two dimensional array
ele'' = reshape (Z :. 2 :. 3 :: DIM2) arr ! (Z :. 1 :. 1)

-- rank :: Shape sh => sh -> Int
-- get number of dimensions
d = rank (Z :. 2 :. 3 :. 4 :: DIM3)

-- size :: Shape sh => sh -> Int
-- get number of elements
s = size (Z :. 2 :. 3 :. 4 :: DIM3)

-- extent :: (Shape sh, Source r e) => Array r sh e -> sh
e = extent arr
