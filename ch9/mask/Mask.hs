module Mask (
    problem'
) where

import Control.Exception (mask, catch, throw, SomeException)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newEmptyMVar, newMVar)

problem :: MVar a -> (a -> IO a) -> IO ()
problem m f =
    takeMVar m >>= \a -> -- <1>
    f a `catch` (\e -> putMVar m a >> throw (e :: SomeException)) >>= \r -> -- <2>
    putMVar m r -- <3>

-- If an asynchronous exception strikes between 1 and 2 or between 2 and 3,
-- the MVar will be left empty. In fact, there is no way to shuffle around the
-- exception handlers to ensure the MVar is always left full.

-- To fix this problem, Haskell provides the mask combinator:
-- mask :: ((IO a -> IO a) -> IO b) -> IO b
problem' :: MVar a -> (a -> IO a) -> IO ()
problem' m f = mask $ \restore ->
    takeMVar m >>= \a ->
    -- restore :: IO a -> IO a
    -- The restore function can be used to restore the delivery of asynchronous exceptions to its
    -- present state during execution of the argument to  mask.
    -- restore的作用可以类似认为使得mask的参数的所有异常都会在restore的参数(f a)处抛出
    -- 即把asynchronous exception，不知道何时抛出的异常都放到(f a)处抛出
    restore (f a) `catch` (\e -> putMVar m a >> throw (e :: SomeException)) >>= \r ->
    putMVar m r

-- 如果在takeMVar处block，进程不能够被杀死，因为asynchronous exception不会被抛出；
-- 导致无意义的block；
-- 幸好takeMVar被设计成interruptible；
-- Interruptible operations may receive asynchronous exceptions even inside mask. 

-- However, thanks to a subtlety in the precise definition of interruptibility, we are still
-- safe. An interruptible operation may receive an asynchronous exception only if it actually blocks.
-- 只有interruptible operations真正处于block状态的时候，才能接收异常；
-- 比如takeMVar因为所取MVar没有值而block，此时退出不会导致deadlock问题。

-- The guarantee therefore relies on the  MVar being operated in a consistent way,
-- where every operation consists of  takeMVar followed by  putMVar. This is a common
-- requirement for many  MVar operations—a particular use of  MVar comes with a protocol
-- that operations must follow or risk a deadlock.
-- 上面的保证仅仅在于我们遵循一定的编码规范的时候才成立。

-- When you really need to call an interruptible function but can’t af‐
-- ford the possibility that an asynchronous exception might be raised,
-- there is a last resort:
-- uninterruptibleMask :: ((IO a -> IO a) -> IO b) -> IO b
-- This works just like  mask , except that interruptible operations may not
-- receive asynchronous exceptions.
-- 如果不想冒deadlock的风险，可以用这个函数；
-- 使得interruptible operations不会收到异步异常。
