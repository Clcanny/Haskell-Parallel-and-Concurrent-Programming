-- 这个版本的问题是getMessage在收到factor改变的消息之后，
-- 本来要通知所有的talk线程（通过信道），
-- 但是，这样涉及到信道的管理（添加，删除等），比较麻烦，
-- 故没有这样实现。

{-# LANGUAGE ScopedTypeVariables #-}

import ConcurrentUtils (forkFinally)
import Network
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import System.IO (Handle, BufferMode(..), hClose, hSetBuffering, hGetLine, hPutStr, hPutStrLn)
import Text.Printf (printf)
import Control.Exception (SomeException, handle)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              
  printf "Listening on port %d\n" port
  forever $
    accept sock >>= \(handler, host, port) ->
    printf "Accepted connection from %s: %s\n" host (show port) >>
    newChan >>= \state ->
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
      forkFinally (talk handler (State state)) exceptionHandler >>
      forkFinally (getMessage handler (State state)) exceptionHandler

port :: Int
port = 44444
-- >>

data Message =
    FactorChange Integer
  | ClientInput Integer
  | ClientEnd

data State = State {
  clientChans :: Chan Message
}

getMessage :: Handle -> State -> IO ()
getMessage h state = 
  hSetBuffering h LineBuffering >>
  let
    chans = clientChans state
  in
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
              writeChan chans ClientEnd
            else 
              if head line == '*'
                then 
                  let 
                    newFactor = read (drop 1 line) :: Integer
                  in
                    printf "%d\n" newFactor >>
                    writeChan chans (FactorChange newFactor) >> 
                    loop
                else 
                  let
                    num = read line :: Integer
                  in
                    printf "%d\n" num >>
                    writeChan chans (ClientInput (read line :: Integer)) >> 
                    loop
    in
      loop

talk :: Handle -> State -> IO ()
talk h state =
  hSetBuffering h LineBuffering >>
  newMVar (2 :: Integer) >>= \factor ->
  let
    chans = clientChans state
  in
    let
      loop =
        readChan chans >>= \msg ->
        handle (\(_ :: SomeException) -> hClose h) $
          case msg of
            FactorChange newFactor ->
              takeMVar factor >>
              putMVar factor newFactor >>
              hPutStrLn h "factor changed." >>
              loop
            ClientInput num -> 
              readMVar factor >>= \factor' ->
              hPutStrLn h (show (factor' * num)) >>
              loop
            ClientEnd -> 
              hPutStrLn h "Thanks for using Haskell-Doubling services." >>
              hClose h
    in
      loop
