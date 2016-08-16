import Control.Concurrent.STM (STM, retry, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent

type TStream a = TVar (TItem a)
-- If we use MVar, we can use empty MVar to replace tail,
-- but we use TVar now, there is not empty TVar.
-- So we need a special tail.
data TItem a = TNil | TCons a (TStream a)

data TChan a = TChan (TVar (TStream a)) (TVar (TStream a))

newTChan :: STM (TChan a)
newTChan =
    newTVar TNil >>= \hole ->
    newTVar hole >>= \readVar ->
    newTVar hole >>= \writeVar ->
    return (TChan readVar writeVar)

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) val = 
    newTVar TNil >>= \newHole ->
    readTVar writeVar >>= \oldHole ->
    writeTVar oldHole (TCons val newHole) >>
    writeTVar writeVar newHole

readTChan :: TChan a -> STM a
readTChan (TChan readVar _) =
    readTVar readVar >>= \stream ->
    readTVar stream >>= \head ->
        case head of
            TNil -> retry
            TCons val tail ->
                writeTVar readVar tail >>
                return val

dupChan :: TChan a -> STM (TChan a)
dupChan (TChan _ writeVar) = 
    readTVar writeVar >>= \hole ->
    newTVar hole >>= \newReaderVar ->
    return (TChan newReaderVar writeVar)

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan readVar _) val =
    readTVar readVar >>= \readEnd ->
    newTVar (TCons val readEnd) >>= \newReadEnd ->
    writeTVar readVar newReadEnd

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _) =
    readTVar read >>= \listHead ->
    readTVar listHead >>= \head ->
    case head of
        TNil -> return True
        TCons _ _ -> return False

-- test
a = atomically $
    newTChan >>= \chan ->
    writeTChan chan 1 >>
    unGetTChan chan 2 >>
    readTChan chan

b = atomically $
    newTChan >>= \chan ->
    unGetTChan chan 1 >>
    readTChan chan

noDeadlock =
    atomically newTChan >>= \chan ->
    forkIO (atomically (readTChan chan) >> putStrLn "I' m done!") >>
    forkIO (atomically (writeTChan chan 1)) >>
    getLine

-- STM是没有真正意义上的锁的，TVar从创建到被GC回收的时间里，都只有满这一个状态。
-- 所以，很自然地帮我们避免了死锁的问题。
noDeadlock' =
    atomically newTChan >>= \chan ->
    forkIO (atomically (readTChan chan) >> putStrLn "I' m done!") >>
    forkIO (atomically (unGetTChan chan 1) >> putStrLn "I'm done too.") >>
    getLine
