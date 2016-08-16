import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = 
  newTVar [] >>= \read ->
  newTVar [] >>= \write ->
  return (TQueue read write)

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _ write) val =
  readTVar write >>= \write' ->
  writeTVar write (val : write')

readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) =
  readTVar read >>= \read' ->
  case read' of
    (x : read') -> writeTVar read read' >> return x
    [] ->
      readTVar write >>= \write' ->
        case write' of
          [] -> retry
          -- 摊销分析
          _ -> 
            let (z : zs) = reverse write'
            in writeTVar write [] >>
            writeTVar read zs >>
            return z

isListEmpty :: [a] -> Bool
isListEmpty = undefined

isTQueueEmpty :: TQueue a -> STM Bool
isTQueueEmpty (TQueue read write) =
  readTVar read >>= \read' ->
  readTVar write >>= \write' ->
  return $ and [isListEmpty read', isListEmpty write']
