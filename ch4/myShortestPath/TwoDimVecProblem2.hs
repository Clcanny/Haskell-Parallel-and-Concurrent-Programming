module TwoDimVec where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import Prelude hiding (read)
import GHC.ST (ST)
import Control.Monad.Primitive (PrimMonad, PrimState)

type MVector' s a = MVector (PrimState (ST s)) a
-- MMVector = MonadMVector
type MMVector s a = ST s (MVector' s a)
-- TDMVector = two dimensional mutable vector
data TDMVector s a = TDMVector {
      maxRow :: Int
    , readVec :: MVector' s a
}
type MTDMVector s a = ST s (TDMVector s a)
data TDIVector s a = TDIVector {
      maxRow' :: Int
    , readVec' :: Vector (MMVector s a)
}
type MTDIVector s a = ST s (TDIVector s a)

constVec :: Int -> Int -> MTDMVector s a
constVec col row = 
    MVector.new (col * row) >>= \vec ->
    return $ TDMVector row vec

-- read :: TMVector a -> Int -> Int -> PrimMonad' a
-- read vec col row = 
--     MVector.read vec col >>= id >>= \vec' ->
--     MVector.read vec' row

-- write :: TMVector a -> Int -> Int -> a -> PrimMonad' ()
-- write vec col row a =
--     MVector.read vec col >>= id >>= \vec' ->
--     MVector.write vec' row a

create :: MTDMVector s a -> MTDIVector s a
create mvec = TDIVector
    where 
        ivec = 
            Vector.create $ do
                mvec' <- mvec
                let vec = readVec mvec'
                return vec
        row = 

t :: Vector (MMVector s a)
t = Vector.create $ do
    vec <- constVec 1 1
    let vec' = readVec vec
    return vec'
