import Data.Array.Repa (
    Z(..)
  , (:.)(..)
  , fromListUnboxed
  , fromFunction
  , computeS
  , DIM1, DIM2, DIM3
  , Array, U, D
  , (!)
  , extent
  , Shape
  , Source
  )
import qualified Data.Array.Repa as Repa

-- Repa.map :: (Shape sh, Source r a) => (a -> b) -> Array r sh a -> Array D sh b
arr = fromListUnboxed (Z :. 10 :: DIM1) ([1..10] :: [Int])
mArr = Repa.map (+ 1) arr

-- computeS :: (Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e
cArr :: Array U DIM1 Int
cArr = computeS mArr
-- The most important instances of the Load class and the Target class are D and
-- U respectively; that is, computeS turns a delayed array into a concrete
-- unboxed array.

-- You might be wondering why there is this extra complication — why doesn’t map just
-- produce a new array? The answer is that by representing the result of an array operation
-- as a delayed array, a sequence of array operations can be performed without
-- ever building the intermediate arrays; this is an optimization called fusion, and it’s
-- critical to achieving good performance with Repa.
noIntermediateArray = computeS (Repa.map (+ 1) (Repa.map (^ 2) arr)) :: Array U DIM1 Int
-- The intermediate array between the two maps is not built, and in fact if we compile this
-- rather than running it in GHCi, provided the optimization option -O is enabled, it will
-- compile to a single efficient loop over the input array.

-- How the computeS works?
-- The fundamental way to get a delayed array is fromFunction:
-- fromFunction :: sh -> (sh -> a) -> Array D sh a
dArr = fromFunction (Z :. 10) (\(Z :. i) -> i :: Int)
dArr' = fromFunction (Z :. 10) (\(Z :. i) -> i + 1 :: Int)

mymap :: (Shape sh, Source r e) => (e -> a) -> Array r sh e -> Array D sh a
mymap f a = fromFunction (extent a) (\ix -> f (a ! ix))
-- What happens if we compose two  map s together? The result would be a delayed array
-- containing a function that indexes into another delayed array. So we’re
-- building up a nested function that defines the array elements, rather than intermediate
-- arrays. Furthermore, Repa is carefully engineered so that at compile time the nested
-- function call is optimized away as far as possible, yielding very efficient code.
