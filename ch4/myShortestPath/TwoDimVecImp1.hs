module TwoDimVecImp1 (
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

data MixType a = MaxRow Int | Elem a

unsafeGetMaxRow :: MixType a -> Int
unsafeGetMaxRow (MaxRow i) = i

unsafeGetElem :: MixType a -> a
unsafeGetElem (Elem a) = a

type PrimMonad' = IO
type MVector' a = MVector (PrimState PrimMonad') a
-- MMVector = MonadMVector
type MMVector a = PrimMonad' (MVector' a)
type MTDMVector a = MMVector (MixType a)

constVec :: Int -> Int -> a -> MTDMVector a
constVec col row a = 
    MVector.replicate (col * row + 1) (Elem a) >>= \vec ->
    MVector.write vec 0 (MaxRow row) >>
    return vec

read :: MVector' (MixType a) -> Int -> Int -> PrimMonad' a
read vec col row = 
        MVector.read vec realIndex >>= \res' ->
        return (unsafeGetElem res')
    where
        rowMax = unsafeGetMaxRow $ unsafePerformIO (MVector.read vec 0)
        realIndex = (col - 1) * rowMax + row

read' :: MTDMVector a -> Int -> Int -> a
read' vec col row =
    

write :: MVector' (MixType a) -> Int -> Int -> a -> PrimMonad' ()
write vec col row a = MVector.write vec realIndex (Elem a)
    where
        rowMax = unsafeGetMaxRow $ unsafePerformIO (MVector.read vec 0)
        realIndex = (col - 1) * rowMax + row

toList :: MVector' (MixType a) -> Int -> [a] -> [a]
toList vec n list
    | n < 0 = list
    | otherwise = toList vec (n - 1) (unsafeGetElem (unsafePerformIO (MVector.read vec n)) : list)

toListHelper :: MVector' (MixType a) ->  Int -> [[a]] -> [[a]]
toListHelper vec len list
    | vecLen == 0 = list
    | otherwise = toListHelper headPart len (toList tailPart (len - 1) [] : list)
    where
        vecLen = MVector.length vec
        start = vecLen - len
        (headPart, tailPart) = MVector.splitAt start vec

toList' :: MVector' (MixType a) -> [[a]]
toList' vec = toListHelper other first' []
    where
        (first, other) = MVector.splitAt 1 vec
        first' = unsafeGetMaxRow $ unsafePerformIO (MVector.read first 0)

-- test
xs = constVec 4 3 "0" :: MTDMVector String
ys = 
    xs >>= \xs' ->
    write xs' 1 1 "1" >>
    write xs' 2 1 "2" >>
    write xs' 3 1 "3" >>
    write xs' 4 1 "4" >>
    write xs' 4 2 "5" >>
    return (toList' xs')
