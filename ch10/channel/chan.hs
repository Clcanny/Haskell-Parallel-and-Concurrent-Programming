import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)

-- MVar (Item a, MVar (Item a, MVar ...))
-- end with a empty MVar.
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
    -- end with an empty MVar
    putMVar oldHole (Item val newHole) >>
    putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan readVar _) =
    takeMVar readVar >>= \stream ->
    -- type Stream a = MVar (Item a)
    -- takeMvar stream = Item a
    -- consider dupChan, u can't discard stream after read it.
    readMVar stream >>= \(Item val tail) ->
    putMVar readVar tail >>
    return val

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
    writeChan chan 1 >>
    unGetChan chan 2 >>
    readChan chan

b =
    newChan >>= \chan ->
    unGetChan chan 1 >>
    readChan chan

noDeadlock =
    newChan >>= \chan ->
    forkIO (readChan chan >> putStrLn "I' m done!") >>
    forkIO (writeChan chan 1) >>
    getLine

deadlock =
    newChan >>= \chan ->
    forkIO (readChan chan >> putStrLn "I' m done!") >>
    forkIO (unGetChan chan 1 >> putStrLn "I'm done too.") >>
    getLine
