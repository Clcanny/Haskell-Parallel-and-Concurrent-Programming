module TwoDimVec where

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

constVec :: Int -> Int -> Int -> MMVector Int
constVec col row a = 
    MVector.replicate (col * row + 1) a >>= \vec ->
    MVector.write vec 0 row >>
    return vec

read :: MVector' Int -> Int -> Int -> PrimMonad' Int
read vec col row = MVector.read vec realIndex
    where
        rowMax = unsafePerformIO (MVector.read vec 0)
        realIndex = (col - 1) * rowMax + row

write :: MVector' Int -> Int -> Int -> Int -> PrimMonad' ()
write vec col row a = MVector.write vec realIndex a
    where
        rowMax = unsafePerformIO (MVector.read vec 0)
        realIndex = (col - 1) * rowMax + row

toList :: MVector' Int -> Int -> [Int] -> [Int]
toList vec n list
    | n < 0 = list
    | otherwise = toList vec (n - 1) (unsafePerformIO (MVector.read vec n) : list)

toListHelper :: MVector' Int ->  Int -> [[Int]] -> [[Int]]
toListHelper vec len list
    | vecLen == 0 = list
    | otherwise = toListHelper headPart len (toList tailPart (len - 1) [] : list)
    where
        vecLen = MVector.length vec
        start = vecLen - len
        (headPart, tailPart) = MVector.splitAt start vec

toList' :: MVector' Int -> [[Int]]
toList' vec = toListHelper other first' []
    where
        (first, other) = MVector.splitAt 1 vec
        first' = unsafePerformIO (MVector.read first 0)

-- test
xs = constVec 4 3 0
x = xs >>= \xs' -> write xs' 1 1 6 >> read xs' 1 1 -- yes
y = xs >>= \xs' -> read xs' 1 1 -- no
ys = xs >>= \xs' -> return $ toList xs' 12 []
zs = xs >>= \xs' -> return (MVector.drop 1 xs') >>= \xs'' -> return $ toList xs'' 11 []
as = xs >>= \xs' -> return (MVector.drop 1 xs') >>= \xs'' -> return $ toListHelper xs'' 3 []
bs = xs >>= \xs' -> return $ toList' xs'
cs = xs >>= \xs' -> write xs' 2 1 2 >> write xs' 3 1 3 >> return (toList' xs')
ds = 
    xs >>= \xs' -> 
    write xs' 1 1 1 >>
    write xs' 2 1 2 >>
    write xs' 3 1 3 >>
    write xs' 4 1 4 >>
    return (toList xs' 12 [])
es = 
    xs >>= \xs' -> 
    write xs' 1 1 1 >>
    return (toList xs' 12 [])
fs = 
    xs >>= \xs' ->
    write xs' 1 1 1 >>
    write xs' 2 1 2 >>
    write xs' 3 1 3 >>
    write xs' 4 1 4 >>
    return (toList' xs')
