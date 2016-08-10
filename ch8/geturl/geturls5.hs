import Control.Concurrent
import GetURL
import qualified Data.ByteString as B
import Text.Printf
import Control.Monad

sites = ["http://www.baidu.com",
         "http://www.sohu.com",
         "http://www.sina.com"]

download :: MVar (String, B.ByteString) -> String -> IO ()
download m url =
    getURL url >>= \r ->
    putMVar m (url, r)

main :: IO ()
main =
    newEmptyMVar >>= \m ->
    mapM_ (forkIO . (download m)) sites >>
    takeMVar m >>= \(url, r) ->
    printf "%s was first (%d bytes)\n" url (B.length r) >>
    -- 如果不做这一步，会导致子进程的死锁，MVar再装一个值就永远是满的
    replicateM_ (length sites - 1) (takeMVar m)

myWaitAny :: [MVar a -> IO ()] -> IO a
myWaitAny xs = 
    newEmptyMVar >>= \m ->
    -- forkIO :: IO () -> IO ThreadId
    -- map :: (a -> b) -> [a] -> [b]
    -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
    mapM_ forkIO (map (\f -> f m) xs) >>
    takeMVar m 
    -- maybe cause deadlock

myWaitAny' :: [MVar a -> IO ()] -> IO a
myWaitAny' xs =
    newEmptyMVar >>= \m ->
    mapM_ forkIO (map (\f -> f m) xs) >>
    takeMVar m >>= \res ->
    -- no deadlock
    replicateM_ (length xs - 1) (takeMVar m) >>
    return res

download' :: String -> MVar (String, B.ByteString) -> IO ()
download' url m = download m url

main' :: IO ()
main' =
    myWaitAny' (map download' sites) >>= \(url, r) -> 
    printf "%s was first (%d bytes)\n" url (B.length r)

download'' :: String -> IO (String, B.ByteString)
download'' url =
    getURL url >>= \r ->
    return (url, r)

mapHelper :: MVar a -> IO a -> IO ()
mapHelper m fx = 
    fx >>= \x' ->
    putMVar m x'

myWaitAny'' :: [IO a] -> IO a
myWaitAny'' xs =
    newEmptyMVar >>= \m ->
    mapM_ forkIO (map (mapHelper m) xs) >>
    takeMVar m >>= \res ->
    replicateM_ (length xs - 1) (takeMVar m) >>
    return res

main'' :: IO ()
main'' =
    myWaitAny'' (map download'' sites) >>= \(url, r) ->
    printf "%s was first (%d bytes)\n" url (B.length r)
