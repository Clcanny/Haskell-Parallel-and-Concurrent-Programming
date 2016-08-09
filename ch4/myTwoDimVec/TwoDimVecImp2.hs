module TwoDimVecImp2 (
      PrimMonad'
    , MTDMVector
    , constVec
    , read
    , write
    , toList'
) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import Prelude hiding (read)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive (PrimMonad, PrimState)

type PrimMonad' = IO
type MVector' a = MVector (PrimState PrimMonad') a
-- MMVector = MonadMVector
type MMVector a = PrimMonad' (MVector' a)
-- TDMVector = two dimensional mutable vector
data TDMVector a = TDMVector {
      maxRow :: Int
    , readVec :: MVector' a
}
-- monad, two, dimensional, mutable, vector
type MTDMVector a = PrimMonad' (TDMVector a)

constVec :: Int -> Int -> a -> MTDMVector a
constVec col row a = 
    MVector.replicate (col * row) a >>= \vec ->
    return $ TDMVector row vec

read :: TDMVector a -> Int -> Int -> PrimMonad' a
read vec col row = MVector.read vec' realIndex
    where 
        maxRow' = maxRow vec
        vec' = readVec vec
        realIndex = (col - 1) * maxRow' + row - 1

write :: TDMVector a -> Int -> Int -> a -> PrimMonad' ()
write vec col row a = MVector.write vec' readIndex a
    where
        maxRow' = maxRow vec
        vec' = readVec vec
        readIndex = (col - 1) * maxRow' + row - 1

toList :: MVector' a -> Int -> [a] -> [a]
toList vec n list
    | n < 0 = list
    | otherwise = toList vec (n - 1) (unsafePerformIO (MVector.read vec n) : list)

toListHelper :: MVector' a -> Int -> [[a]] -> [[a]]
toListHelper vec len list
    | vecLen == 0 = list
    | otherwise = toListHelper headPart len (toList tailPart (len - 1) [] : list)
    where
        vecLen = MVector.length vec
        start = vecLen - len
        (headPart, tailPart) = MVector.splitAt start vec

toList' :: TDMVector a -> [[a]]
toList' vec = toListHelper vec' maxRow' []
    where
        maxRow' = maxRow vec
        vec' = readVec vec

-- test
xs = constVec 4 5 0 :: MTDMVector Int

ys = 
    xs >>= \xs' ->
    write xs' 1 1 1 >>
    write xs' 2 1 2 >>
    write xs' 3 1 3 >>
    write xs' 4 1 4 >>
    write xs' 4 2 5 >>
    return (toList' xs')
