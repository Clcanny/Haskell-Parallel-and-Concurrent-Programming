-- 这个版本的问题是：
-- 如果一个客户端改变了factor之后，下一个客户端才连接进来；
-- 此时新客户端的factor仍然是2，直到有新的factor输入。

{-# LANGUAGE ScopedTypeVariables #-}

import ConcurrentUtils (forkFinally)
import Network
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import System.IO (Handle, BufferMode(..), hClose, hSetBuffering, hGetLine, hPutStr, hPutStrLn)
import Text.Printf (printf)
import Control.Exception (SomeException, handle)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, isEmptyChan, dupChan)

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              
  printf "Listening on port %d\n" port
  chans <- newChan
  forever $
    accept sock >>= \(handler, host, port) ->
    printf "Accepted connection from %s: %s\n" host (show port) >>
    dupChan chans >>= \chans' ->
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
      forkFinally (talk handler (State chans')) exceptionHandler

port :: Int
port = 44444
-- >>

newtype State = State {
  broadcastChan :: Chan Integer
}

talk :: Handle -> State -> IO ()
talk h state =
  hSetBuffering h LineBuffering >>
  newMVar 2 >>= \factor ->
  let
    loop =
      hGetLine h >>= \line ->
      handle 
        (\e -> 
          print (e :: SomeException) >> 
          hPutStrLn h "Please input numbers or \"end\"." >> 
          loop) $
        if line == "end"
          then 
            hPutStrLn h "Thank u for using the Haskell doubling service."
          else
            if head line == '*'
              then 
                let
                  newFactor = read (drop 1 line) :: Integer
                in
                  printf "%d\n" newFactor >>
                  writeChan (broadcastChan state) newFactor >>
                  loop
              else
                let 
                  chans = broadcastChan state
                in
                  isEmptyChan chans >>= \isEmpty ->
                  case isEmpty of
                    True -> 
                      readMVar factor >>= \factor' -> 
                      hPutStrLn h (show (factor' * read line ::Integer)) >> 
                      loop
                    False -> 
                      readChan chans >>= \newFactor -> 
                      takeMVar factor >> 
                      putMVar factor newFactor >>
                      hPutStrLn h (show (newFactor * (read line :: Integer))) >>
                      loop
  in 
    loop
