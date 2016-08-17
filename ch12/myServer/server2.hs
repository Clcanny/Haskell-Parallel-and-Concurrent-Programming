import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO)
import System.IO
import Text.Printf
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan

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

data Message =
    FactorChange Integer
  | ClientInput Integer
  | ClientEnd

data State = State {
  clientChans :: [TChan Message]
}

getMessage :: Handle -> IO Message
getMessage h =
  hSetBuffering h LineBuffering >>
  hGetLine h >>= \line ->
  handle
    (\e ->
      printf "Meet " >>
      print (e :: SomeException) >>
      hPutStrLn h "Please input numbers or \"end\"." >>
      getMessage h) $
    if line == "end"
      then return ClientEnd
      else 
        if head line == '*'
          then return (FactorChange (read (drop 1 line) :: Integer))
          else return (FactorInput (read line :: Integer))

talk :: Handle -> Message -> IO ()
talk h mess =
  hSetBuffering h LineBuffering >>
  newMVar (2 :: Integer) >>= \factor ->
  handle (\_ -> hClose h) $
    case mess of
      FactorChange newFactor ->
        takeMVar factor >>
        putMVar factor newFactor
      ClientInput num -> 
        readMVar factor >>= \factor' ->
        hPutStrLn h (show (factor' * num))
      ClientEnd -> hClose h

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
