module TwoDimVecImp3 where

import Interface
import ReaderT

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import Prelude hiding (read)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive (PrimMonad, PrimState)

type PrimMonad' = IO
type MVector' a = MVector (PrimState PrimMonad') a
-- type MMVector a = PrimMonad' (MVector' a)
type TDMVector a = ReaderT Int PrimMonad' (MVector' a)
-- type MTDMVector a = PrimMonad' (TDMVector a)

constVec :: Int -> Int -> a -> TDMVector a
constVec col row a = 
    lift $ MVector.replicate (col * row) a

read :: TDMVector a -> Int -> Int -> a
read vec col row = do
    maxRow <- ask
    let realIndex = (col - 1) * maxRow + row - 1
    vec' <- vec
    vec'' <- vec'
    lift $ MVector.read vec'' realIndex
