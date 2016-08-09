import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)

main =
    newEmptyMVar >>= \m ->
    forkIO (putMVar m "hello" >> putMVar m "hi") >>
    takeMVar m >>= \r ->
    print r >>
    takeMVar m >>= \r ->
    print r
