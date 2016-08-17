import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO)
import System.IO
import Text.Printf
import Control.Exception

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

-- <<talk
talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
 where
  loop = do
    line <- hGetLine h
    if line == "end"
       then hPutStrLn h ("Thank you for using the " ++
                         "Haskell doubling service.")
       else do hPutStrLn h (show (2 * (read line :: Integer)))
               loop
-- >>

talk' :: Handle -> IO ()
talk' h = do
  hSetBuffering h LineBuffering
  loop
 where
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
          hPutStrLn h (show (2 * (read line :: Integer))) >>
          loop
