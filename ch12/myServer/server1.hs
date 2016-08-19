-- 这个版本的缺点是没有实现handle的管理，
-- 不能通知所有的客户端，factor已经改变，即该改变静默发生。

import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO)
import System.IO
import Text.Printf
import Control.Exception
import Control.Concurrent.MVar

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              
  printf "Listening on port %d\n" port
  state <- newMVar (State 2 [])
  forever $
    accept sock >>= \(handler, host, port) ->
    printf "Accepted connection from %s: %s\n" host (show port) >>
    let
      exceptionHandler r = 
        case r of
          Left e -> 
            printf "Meet an Exception.\n" >> 
            hClose handler
          Right res -> 
            printf "Meet an End.\n" >> 
            hClose handler
    in
      forkFinally (talk' (StateVar state) handler) exceptionHandler

port :: Int
port = 44444
-- >>

data State = State {
  currentFactor :: Integer,
  clientHandles :: [Handle]
}

newtype StateVar = StateVar {
  runStateVar :: MVar State
}

talk' :: StateVar -> Handle -> IO ()
talk' state h = do
  hSetBuffering h LineBuffering
  let
    loop =
      hGetLine h >>= \line ->
      handle 
        (\e -> 
          printf "Meet " >> 
          print (e :: SomeException) >> 
          hPutStrLn h "Please input numbers or \"end\"." >> 
          loop) $
        if line == "end"
          then hPutStrLn h "Thank u for using the Haskell doubling service."
          else
            if head line == '*'
              then 
                let 
                  newFactor = read (drop 1 line) :: Integer
                in
                  takeMVar (runStateVar state) >>
                  putMVar (runStateVar state) (State newFactor []) >>
                  loop
              else
                readMVar (runStateVar state) >>= \(State factor _) ->
                hPutStrLn h (show (factor * (read line :: Integer))) >>
                loop
  loop
