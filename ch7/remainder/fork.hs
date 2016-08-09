import Control.Concurrent
import Control.Monad
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    forkIO (replicateM_ 100 (putChar 'a'))
    replicateM_ 100 (putChar 'b')
