import Control.Concurrent.STM (STM, retry, atomically, throwSTM, orElse, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException, Exception, mask, try, throwIO, throwTo, catch)

import GetURL
import qualified Data.ByteString as B

-- >>= CancelException

import Data.Typeable (Typeable)
data CancelException = CancelException deriving (Show, Typeable)
instance Exception CancelException

-- =<< CancelException

-- >>= TMVar

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

-- =<< TMVar

-- >>= helper function

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action after =
  mask $ \restore ->
    forkIO $ try (restore action) >>= after

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after during = mask $ \restore ->
    before >>= \a ->
    restore (during a) `catch` (\e -> after a >> throwIO (e :: SomeException)) >>= \r ->
    after a >>
    return r

-- =<< helper function

-- >>= Async

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action =
  atomically newEmptyTMVar >>= \var ->
  forkFinally action (atomically . putTMVar var) >>= \t ->
  return (Async t var)

cancel :: Async a -> IO ()
cancel (Async id _) = throwTo id CancelException

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
wait a =
  waitCatch a >>= \r ->
  case r of
    Left e -> throwIO e
    Right a -> return a

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a) `orElse` fmap Right (waitSTM b)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a1 a2 = atomically $
  waitSTM a1 `orElse` (waitSTM a2 >> retry) >>= \r1 ->
  waitSTM a2 >>= \r2 ->
  return (r1, r2)

waitAny :: [Async a] -> IO a
waitAny asyncs = atomically $
  foldr orElse retry $ map waitSTM asyncs

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io operation = bracket (async io) cancel operation

-- =<< Async

main =
  withAsync (getURL "http://www.wikipedia.org/wiki/Shovel") $ \a1 ->
  withAsync (getURL "http://www.wikipedia.org/wiki/Spade")  $ \a2 ->
  wait a1 >>= \r1 ->
  wait a2 >>= \r2 ->
  print (B.length r1, B.length r2)
