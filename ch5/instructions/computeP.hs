import Data.Array.Repa
import Data.Functor.Identity

arr :: Array D DIM1 Int
arr = fromFunction (Z :. 5) (\(Z :. i) -> i)



parr :: Array U DIM1 Int
parr = runIdentity $ computeP arr

arr2 :: Array D DIM1 Int
arr2 = fromFunction (Z :. 5) (\ix -> parr ! ix)

-- problem here
parr2 :: Array U DIM1 Int
parr2 = runIdentity $ computeP arr2



parr' :: Identity (Array U DIM1 Int)
parr' = computeP arr

arr2' :: Identity (Array D DIM1 Int)
arr2' = 
  parr' >>= \p ->
  return $ fromFunction (Z :. 5) (\ix -> p ! ix)

parr2' :: Identity (Array U DIM1 Int)
parr2' =
  arr2' >>= \a ->
  computeP a

res = runIdentity parr2'

-- So this is the rule of thumb: if your program makes multiple calls to
-- computeP , try to ensure that they are performed in the same monad.
