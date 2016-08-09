-- 抛弃直接调用进程的思想.
-- 接收只需要通信手段MVar.

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)

-- Logger is a contaniner.
-- Logger把一个信道封装起来.
data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = 
    newEmptyMVar >>= \m ->
    let l = Logger m
    in forkIO (logger l) >>
    return l

-- 如果是Message，输出.
-- 如果是Stop，内部还有一个MVar，并且是空的.
-- 需要填入()，防止其他程序被takeMVar阻塞.
logger :: Logger -> IO ()
logger (Logger m) = loop
    where
        loop =
            takeMVar m >>= \cmd ->
            case cmd of
                Message msg ->
                    putStrLn msg >>
                    loop
                Stop s ->
                    putStrLn "logger: stop" >>
                    putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

-- takeMVar是关键，保证loop已经完成.
logStop :: Logger -> IO ()
logStop (Logger m) =
    newEmptyMVar >>= \s ->
    putMVar m (Stop s) >>
    takeMVar s

main :: IO ()
main = 
    initLogger >>= \l ->
    logMessage l "hello" >>
    logMessage l "hi" >>
    logMessage l "bye" >>
    logStop l
