import GetURL
import TimeIt

import Control.Monad
import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    forkIO (do r <- try action; putMVar var r)  -- <1>
    return (Async var)

waitCatch :: Async a -> IO (Either SomeException a) -- <2>
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a -- <3>
wait a = do
    r <- waitCatch a
    case r of
        Left e  -> throwIO e
        Right a -> return a

-- <<waitEither
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
    m <- newEmptyMVar
    forkIO $ do r <- try (fmap Left  (wait a)); putMVar m r
    forkIO $ do r <- try (fmap Right (wait b)); putMVar m r
    wait (Async m)
-- >>

-- 相比于geturls5.hs的waitAny，这里waitAny接受Async a作为参数；
-- 也就是说，进程至少已经被创建，而且进程把结果放入到各自的MVar中；
-- 所以，我们干了一件比较无聊的事情，把计算结果取出来，然后，再放到新的MVar中；
-- 同时，保证，新的MVar只有一个，即具有排序功能。
-- 死锁问题怎么办？不管啦？
-- 死锁问题放到Main函数中去解决了。
-- <<waitAny
waitAny :: [Async a] -> IO a
waitAny as = do
    m <- newEmptyMVar
    -- The code for  waitAny is quite short and does the job, but it is slightly annoying to have
    -- to create an extra thread per  Async for this simple operation. Threads might be cheap,
    -- but we ought to be able to merge multiple sources of events more directly. 
    let forkwait a = forkIO $ do r <- try (wait a); putMVar m r
    mapM_ forkwait as
    wait (Async m)
-- >>

-----------------------------------------------------------------------------

sites = ["http://www.baidu.com",
         "http://www.sohu.com"]
         -- 错误处理机制并不完善，
         -- Main函数中缺乏相应的错误处理函数。
         --"http://www.unknowWebsite.com"]

-- <<main
main :: IO ()
main = do
    let
        download url = do
            r <- getURL url
            return (url, r)

    as <- mapM (async . download) sites

    (url, r) <- waitAny as
    printf "%s was first (%d bytes)\n" url (B.length r)
    -- 防止死锁
    mapM_ wait as
-- >>
