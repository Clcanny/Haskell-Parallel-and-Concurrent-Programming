-- module twoDimArr where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector

import Prelude hiding (read)
import Control.Monad.Primitive (PrimMonad, PrimState)

type PrimMonad' = IO
type MVector' a = MVector (PrimState PrimMonad') a
type TMVector a = MVector' (PrimMonad' (MVector' a))

constArr :: Int -> Int -> PrimMonad' (TMVector a)
constArr col row = MVector.replicate col (MVector.new row)

read :: TMVector a -> Int -> Int -> PrimMonad' a
read vec col row = 
    MVector.read vec col >>= id >>= \vec' ->
    MVector.read vec' row

write :: TMVector a -> Int -> Int -> a -> PrimMonad' ()
write vec col row a =
    MVector.read vec col >>= id >>= \vec' ->
    MVector.write vec' row a
