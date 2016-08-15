-- show how to use retry

import Control.Concurrent.STM (STM, retry, atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar)

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = 
  newTVar Nothing >>= \t ->
  return (TMVar t)

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) =
  -- The retry operation says, “Run the current transaction again,” which will have
  -- the desired effect: we keep rerunning the transaction until the TVar no longer
  -- contains Nothing and the other case branch is taken. 
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

-- So now that we have a replacement for  MVar built using STM, what can we do
-- with it?
-- Well, STM operations are composable, so we can perform operations on multiple
-- TMVar s at the same time:
test ta tb = atomically $ do
  a <- takeTMVar ta
  b <- takeTMVar tb
  return (a,b)
-- This STM transaction succeeds when and only when both TMVar s are full;
-- otherwise it is blocked. This explains why retry must abandon the whole transaction: if
-- the first takeTMVar succeeds but the second one retries, we do not want the effect of
-- the first takeTMVar to take place.
-- 这种完整性能够帮我们简化错误处理等等方面。
