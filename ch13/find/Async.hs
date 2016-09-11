module Async where

import Control.Concurrent.STM (STM, retry, atomically, throwSTM, orElse, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException, mask, try)

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = 
  newTVar Nothing >>= \t ->
  return (TMVar t)

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) =
  readTVar t >>= \m ->
  case m of
    Nothing -> retry
    Just a -> 
      writeTVar t Nothing >>
      return a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do
      writeTVar t (Just a)
      return ()
    Just _ -> retry

data Async a = Async ThreadId (TMVar (Either SomeException a))

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action after =
  mask $ \restore ->
    forkIO $ try (restore action) >>= after

async :: IO a -> IO (Async a)
async action =
  atomically newEmptyTMVar >>= \var ->
  forkFinally action (atomically . putTMVar var) >>= \t ->
  return (Async t var)

-- Right or not?
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync action inner =
  atomically newEmptyTMVar >>= \var ->
  mask $ \restore ->
    try (restore action) >>= \t ->
    atomically . putTMVar var ->
    let a = Async t (readTMVar var)

  async a >>= \b ->
  func b

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = takeTMVar var

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

waitSTM :: Async a -> STM a
waitSTM a =
  waitCatchSTM a >>= \r ->
  case r of
    Left e -> throwSTM e
    Right a -> return a

wait :: Async a -> IO a
wait = atomically . waitSTM

waitAnySTM :: [Async a] -> STM a
waitAnySTM asyncs = 
  foldr orElse retry $
    map waitSTM asyncs

waitAny :: [Async a] -> IO a
waitAny = atomically . waitAnySTM
