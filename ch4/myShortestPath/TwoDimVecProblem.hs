module TwoDimVec where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import Prelude hiding (read)
import Control.Monad.Primitive (PrimMonad, PrimState)

type PrimMonad' = IO
type MVector' a = MVector (PrimState PrimMonad') a
type TMVector a = MVector' (PrimMonad' (MVector' a))

constVec :: Int -> Int -> a -> PrimMonad' (TMVector a)
constVec col row a = MVector.replicate col (MVector.replicate row a)

read :: TMVector a -> Int -> Int -> PrimMonad' a
read vec col row = 
    MVector.read vec col >>= id >>= \vec' ->
    MVector.read vec' row

write :: TMVector a -> Int -> Int -> a -> PrimMonad' ()
write vec col row a = 
    MVector.read vec col >>= id >>= \vec' ->
    MVector.write vec' row a

-- test
xss = constVec 2 3 1 :: PrimMonad' (TMVector Int)
x = 
    xss >>= \xss' ->
    write xss' 1 1 2 >>
    read xss' 1 1

ys = MVector.replicate 3 1 :: PrimMonad' (MVector' Int)
y = 
    ys >>= \ys' -> 
    MVector.write ys' 1 2 >>
    MVector.read ys' 1
