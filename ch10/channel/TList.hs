import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM (STM, retry)

newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
  v <- newTVar []
  return (TList v)

writeTList :: TList a -> a -> STM ()
writeTList (TList v) a = do
  list <- readTVar v
  writeTVar v (list ++ [a])

readTList :: TList a -> STM a
readTList (TList v) = do
  xs <- readTVar v
  case xs of
    [] -> retry
    (x : xs') -> do
      writeTVar v xs'
      return x

isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList _ = False

isEmptyTList :: TList a -> STM Bool
isEmptyTList (TList v) = 
  readTVar v >>= \v' ->
  return (isEmptyList v')
