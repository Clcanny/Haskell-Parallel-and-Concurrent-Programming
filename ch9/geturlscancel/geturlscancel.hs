import GetURL
import TimeIt

import Data.Either
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString as B
import Prelude hiding (catch)

import Data.Typeable (Typeable)
data MyThreadKilled = MyThreadKilled deriving (Show, Typeable)
instance Exception MyThreadKilled

-- add ThreadId here
data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO (do r <- try action; putMVar m r)
   return (Async t m)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var

cancel :: Async a -> IO ()
-- cancel (Async t var) = throwTo t ThreadKilled
cancel (Async t var) = throwTo t MyThreadKilled

-----------------------------------------------------------------------------

sites = ["http://www.baidu.com",
         "http://www.sohu.com",
         "http://www.yahoo.com",
         "http://www.zhihu.com",
         "http://www.qcloud.com",
         "http://www.google.com"]

timeDownload :: String -> IO ()
timeDownload url = do
    (page, time) <- timeit $ getURL url
    printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main = do
    as <- mapM (async . timeDownload) sites 
    forkIO $ do
        hSetBuffering stdin NoBuffering
        forever $ do
            c <- getChar
            when (c == 'q') $ mapM_ cancel as
    rs <- mapM waitCatch as
    printf "%d/%d succeeded\n" (length (rights rs)) (length rs)
