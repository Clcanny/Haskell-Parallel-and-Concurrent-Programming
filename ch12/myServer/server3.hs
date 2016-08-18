import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO)
import System.IO
import Text.Printf
import Control.Exception
import Control.Concurrent.STM.TChan

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              
  printf "Listening on port %d\n" port
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
      forkFinally (talk' handler) exceptionHandler

port :: Int
port = 44444
-- >>

newtype State = State {
  broadcastTChan :: TChan Int
}

talk' :: Handle -> IO ()
talk' h = do
  hSetBuffering h LineBuffering
  loop
 where
  loop =
    hGetLine h >>= \line ->
    handle 
      (\e -> 
        print (e :: SomeException) >> 
        hPutStrLn h "Please input numbers or \"end\"." >> 
        loop) $
      if line == "end"
        then hPutStrLn h "Thank u for using the Haskell doubling service."
        else
          if head line == '*'
            then 
              let 
                factor = broadcastTChan state
                newFactor = read (drop 1 line) :: Integer
              in
                printf "factor: %d\n" newFactor >>
                atomically $ writeTChan factor newFactor
            else

              hPutStrLn h (show (2 * (read line :: Integer))) >>
              loop
