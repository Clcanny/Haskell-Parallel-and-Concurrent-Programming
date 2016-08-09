import Control.Concurrent (forkIO, ThreadId)

x :: Int
x = 1 + 1

y :: IO ()
y = return x >> return ()

z :: IO ThreadId
z = forkIO y
