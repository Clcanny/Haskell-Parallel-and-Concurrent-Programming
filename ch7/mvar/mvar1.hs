import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)

main = 
    newEmptyMVar >>= \m ->
    forkIO $ putMVar m "hello" >>
    takeMVar m >>= \r ->
    print r
