{-# LANGUAGE BangPatterns, CPP #-}

module Stream2
 ( 
   Stream, streamFromList, streamMap, streamFold, runPar
 ) where

import Control.Monad.Par (IVar, Par, new, put, get, fork, runPar)
import Control.DeepSeq

data IList a = Nil | Cons a (IVar (IList a)) | Fork (Par ()) (IList a)
type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  -- We can believe that,
  -- other code will ask to calculate t (just fork p),
  -- so we don't have to fork p here.
  rnf (Fork p (Cons h t)) = rnf h `seq` rnf t

-- fork :: Par () -> Par ()
-- put :: NFData a => IVar a -> a -> Par ()
-- get :: IVar a -> Par a
-- new :: Par (IVar a)

streamFromList :: NFData a => Int -> Int -> [a] -> Par (Stream a)
streamFromList f c xs = 
  new >>= \var ->
  fork (loop f c xs var) >>
  return var
 where
  -- We need to add the fork distance f and the chunk size c as parameters:
  -- Actually, f means how much elements we have made, c means the max number of
  -- elements.
  -- loop :: Int -> Int -> [a] -> IVar (IList a) -> Par ()
  loop _ c [] var = put var Nil
  loop 0 c (x:xs) var = 
    new >>= \tail ->
    let
      -- op :: Par ()
      op = loop c c xs tail
    in
      put var (Fork op (Cons x tail))
  loop f c (x:xs) var =
    new >>= \tail ->
    put var (Cons x tail) >> 
    loop (f - 1) c xs tail

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm =
  get instrm >>= \ilst ->
  case ilst of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t
    Fork p (Cons h t) ->
      fork p >>
      streamFold fn (fn acc h) t

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
      Fork p (Cons h t) ->
        new >>= \newtl ->
        put outstrm (Cons (fn h) newtl) >>
        fork p >>
        loop t newtl
