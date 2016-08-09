import Control.Concurrent (forkIO, threadDelay)
import Text.Printf (printf)

setReminder :: String -> IO ()
setReminder s =
    let 
        t = read s :: Int
    in
        printf "Ok, I will remind you in %d seconds.\n" t >>
        threadDelay (10 ^ 6 * t) >>
        printf "%d seconds is Up!\n" t

main = do
    s <- getLine
    forkIO $ setReminder s
