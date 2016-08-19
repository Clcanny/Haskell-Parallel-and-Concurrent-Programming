{-# LANGUAGE ScopedTypeVariables #-}

import ConcurrentUtils (forkFinally)
import Network
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import System.IO (Handle, BufferMode(..), hClose, hSetBuffering, hGetLine, hPutStr, hPutStrLn)
import Text.Printf (printf)
import Control.Exception (SomeException, handle)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO t a = atomically $ writeTVar t a

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              
  printf "Listening on port %d\n" port
  factor <- newTVarIO (2 :: Integer)
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
      forkFinally (talk handler (State factor)) exceptionHandler

port :: Int
port = 44444
-- >>

newtype State = State {
  currentFactor :: TVar Integer
}

talk :: Handle -> State -> IO ()
talk h state =
  hSetBuffering h LineBuffering >>
  let
    factor = currentFactor state
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
                  writeTVarIO factor newFactor >>
                  loop
              else
                readTVarIO factor >>= \factor' -> 
                hPutStrLn h (show (factor' * read line ::Integer)) >> 
                loop
  in 
    loop
