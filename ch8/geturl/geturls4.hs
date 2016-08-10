import GetURL
import TimeIt

import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString as B

-- we can put an exception into Async now.
data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    -- use try action here.
    -- try :: Exception e => IO a -> IO (Either e a)
    -- 如果不用try，一旦action抛出一个异常，forkIO接受到之后给出提示信息；
    -- 然后立马结束进程，意味着putMVar没有被执行；
    -- 如果main调用takeMVar或者readMVar，将导致死锁
    forkIO (do r <- try action; putMVar var r)
    return (Async var)

wait :: Async a -> IO a
wait (Async var) = 
    readMVar var >>= \r ->
    case r of
        Left e -> throwIO e
        Right a -> return a

-- compare with geturls3.hs
sites = ["http://www.baidu.com",
         "http://www.unknownWebsite.com"]

timeDownload :: String -> IO ()
timeDownload url = do
    (page, time) <- timeit $ getURL url 
    printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
    as <- mapM (async . timeDownload) sites 
    mapM_ wait as 
