{-# LANGUAGE BangPatterns, CPP #-}

module Stream
 ( 
   Stream, streamFromList, streamMap, streamFold, runPar
 ) where

import Control.Monad.Par (IVar, Par, new, put, get, fork, runPar)
import Control.DeepSeq

data IList a = Nil | Cons a (IVar (IList a))
type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = 
  new >>= \var ->
  fork (loop xs var) >>
  return var
 where
  loop [] var = put var Nil
  loop (x : xs) var = 
    new >>= \tail ->
    put var (Cons x tail) >>
    loop xs tail

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm =
  get instrm >>= \ilst ->
  case ilst of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm =
  new >>= \outstrm ->
  fork (loop instrm outstrm) >>
  return outstrm
 where
  loop instrm outstrm =
    get instrm >>= \ilst ->
    case ilst of
      Nil -> put outstrm Nil
      Cons h t ->
        new >>= \newtl ->
        put outstrm (Cons (fn h) newtl) >>
        loop t newtl
