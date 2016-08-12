import Mask (problem')
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, throw, mask)

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ = problem'

modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar m f = mask $ \restore ->
    takeMVar m >>= \a ->
    restore (f a) `catch` (\e -> putMVar m a >> throw (e :: SomeException)) >>= \(x, y) ->
    putMVar m x >>
    return y

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after during = mask $ \restore ->
    before >>= \a ->
    restore (during a) `catch` (\e -> after a >> throw (e :: SomeException)) >>= \r ->
    after a >>
    return r

casMVar :: Eq a => MVar a -> a -> a -> IO Bool
casMVar m old new =
    modifyMVar m $ \cur ->
        if cur == old
            then return (new,True)
            else return (cur,False)
