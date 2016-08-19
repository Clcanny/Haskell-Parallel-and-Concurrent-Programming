import Network
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import ConcurrentUtils (forkFinally)

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  factor <- atomically $ newTVar 2                               -- <1>
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle factor) (\_ -> hClose handle)       -- <2>

port :: Int
port = 44444
-- >>

-- <<talk
talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan              -- <1> 用于连接receive和server
  race (server h factor c) (receive h c)  -- <2> 使得server和receive具有兄弟关系（共存亡）
  return ()
-- >>

-- <<receive
receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line
-- >>

-- <<server
server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor     -- <1> 读取factor
  hPrintf h "Current factor: %d\n" f    -- <2> 给新的客户端一点提示
  loop f                                -- <3> 进入循环
 where
  loop f = do
    action <- atomically $ do           -- <4> 把组合后的STM动作转发到IO Monad中
      f' <- readTVar factor             -- <5> 读取factor
      if (f /= f')                      -- <6> 看看factor有没有变化
         then return (newfactor f')     -- <7> factor如果有变化则带着新的factor进入下一个循环
         else do
           l <- readTChan c             -- <8> factor如果没有变化则开始读取消息
           return (command f l)         -- <9> 处理消息
    action

  newfactor f = do                      -- <10> 提示客户端factor已经改变
    hPrintf h "new factor: %d\n" f
    loop f

  command f s                           -- <11> 处理命令
   = case s of
      "end" ->
        hPutStrLn h ("Thank you for using the " ++
                     "Haskell doubling service.")         -- <12> 结束服务
      '*':s -> do
        atomically $ writeTVar factor (read s :: Integer) -- <13> 比if更好的处理方式，'*':s是很巧的方法
        loop f
      line  -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f
-- >>
