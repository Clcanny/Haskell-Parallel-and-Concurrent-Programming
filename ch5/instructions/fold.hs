import Data.Array.Repa
import Data.Functor.Identity

-- foldS
--   :: (Shape sh, Source r a, Data.Vector.Unboxed.Base.Unbox a,
--      repa-3.4.1.1:Data.Array.Repa.Eval.Elt.Elt a) =>
--      (a -> a -> a) -> a -> Array r (sh :. Int) a -> Array U sh a

-- The input array. Note that the shape is (sh :. Int), which means that this is
-- an array of some shape sh with one more dimension.

-- The output array has shape  sh ; that is, one dimension fewer than the input array.

arr :: Array U DIM2 Int
arr = fromListUnboxed (Z :. 3 :. 4) [1..12]

fArr = foldS (\x y -> x + y) 0 arr

arr2 :: Array U DIM3 Int
arr2 = fromListUnboxed (Z :. 2 :. 3 :. 4) [1..24]

fArr2 = foldS (\x y -> x + y) 0 arr2
fArr2' = runIdentity $ foldP (\x y -> x + y) 0 arr2

-- The function argument used with foldP must be associative. That is,
-- the function f must satisfy  f x (f y z) == f (f x y) z. This is
-- because unlike foldS, foldP doesn’t necessarily fold the function over
-- the array elements in strict left-to-right order; it folds different parts
-- of the array in parallel and then combines the results from those parts
-- using the folding function.
