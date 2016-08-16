import Control.Concurrent.STM (STM, retry, atomically, throwSTM, orElse, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException, mask, try)

import GetURL
import TimeIt
import Text.Printf (printf)
import qualified Data.ByteString as B

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

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = takeTMVar var

waitSTM :: Async a -> STM a
waitSTM a =
  waitCatchSTM a >>= \r ->
  case r of
    Left e -> throwSTM e
    Right a -> return a

-- waitEither :: Async a -> Async b -> IO (Either a b)
-- waitEither a b = atomically $
--   return (Left (waitSTM a)) `orElse` return (Right (waitSTM b))

waitAny :: [Async a] -> IO a
waitAny asyncs = atomically $
  -- map waitSTM asyncs :: [STM a]
  -- orElse :: STM a -> STM a -> STM a
  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  -- foldr orElse retry $ map waitSTM asyncs =
  -- async1 `orElse` async2 `orElse` async3 `orElse` ... `orElse` retry
  -- waitSTM调用waitCatchSTM，waitCatchSTM调用takeTMVar，takeTMVar有可能会调用retry
  -- 此时waitAny就能够达到“取出先到的结果”的目的
  foldr orElse retry $ map waitSTM asyncs

sites = ["http://www.google.com",
         "http://www.baidu.com",
         "http://www.yahoo.com"]

main :: IO ()
main = do
  let
    download url = do
       r <- getURL url
       return (url, r)

  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  -- mapM_ wait as
