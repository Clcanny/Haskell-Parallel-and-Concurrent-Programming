import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)
import HighLevelMask (modifyMVar)
import Control.Exception (mask_)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan =
    newEmptyMVar >>= \hole ->
    newMVar hole >>= \readVar ->
    newMVar hole >>= \writeVar ->
    return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = 
    newEmptyMVar >>= \newHole ->
    takeMVar writeVar >>= \oldHole ->
    putMVar oldHole (Item val newHole) >>
    putMVar writeVar newHole

writeChan' :: Chan a -> a -> IO ()
writeChan' (Chan _ writeVar) val =
    newEmptyMVar >>= \newHole ->
    mask_ $
        takeMVar writeVar >>= \oldHole ->
        putMVar oldHole (Item val newHole) >>
        putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan readVar _) =
    takeMVar readVar >>= \stream ->
    readMVar stream >>= \(Item val tail) ->
    putMVar readVar tail >>
    return val

readChan' :: Chan a -> IO a
readChan' (Chan readVar _) = 
    modifyMVar readVar $ \stream ->
        -- 根据我们对readMVar的实现，如果外部异常在这里出现，也会导致deadlock
        -- 正确的实现方式是：
        -- readMVar m = mask_ $ do ...
        readMVar stream >>= \(Item val tail) ->
        return (tail, val)

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = 
    readMVar writeVar >>= \hole ->
    newMVar hole >>= \newReaderVar ->
    return (Chan newReaderVar writeVar)

readMVar :: MVar a -> IO a
readMVar m =
    takeMVar m >>= \a ->
    putMVar m a >>
    return a

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val =
    newEmptyMVar >>= \newReadEnd ->
    takeMVar readVar >>= \readEnd ->
    putMVar newReadEnd (Item val readEnd) >>
    putMVar readVar newReadEnd

-- test
a =
    newChan >>= \chan ->
    writeChan' chan 1 >>
    unGetChan chan 2 >>
    readChan' chan

b =
    newChan >>= \chan ->
    unGetChan chan 1 >>
    readChan' chan

noDeadlock =
    newChan >>= \chan ->
    forkIO (readChan' chan >> putStrLn "I' m done!") >>
    forkIO (writeChan' chan 1 >> putStrLn "I'm done too!") >>
    getLine

deadlock =
    newChan >>= \chan ->
    forkIO (readChan' chan >> putStrLn "I' m done!") >>
    forkIO (unGetChan chan 1 >> putStrLn "I'm done too!") >>
    getLine
