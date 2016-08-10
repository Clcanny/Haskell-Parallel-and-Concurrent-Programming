import GetURL
import TimeIt

import Control.Concurrent
import Text.Printf
import qualified Data.ByteString as B

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    forkIO (do r <- action; putMVar var r)
    return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

sites = ["http://www.baidu.com",
         "http://www.unknowWebsite.com"]

timeDownload :: String -> IO ()
timeDownload url = do
    (page, time) <- timeit $ getURL url 
    printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
    as <- mapM (async . timeDownload) sites 
    mapM_ wait as 
