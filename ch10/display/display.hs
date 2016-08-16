import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Window = Window Int deriving (Eq, Ord, Show)
newtype Desktop = Desktop Int deriving (Eq, Ord, Show)

-- usually block
-- For example, the rendering thread, which needs to look only at the currently displayed desktop, 
-- could be blocked by a window on another desktop that is moving itself. This structure doesn’t
-- allow as much concurrency as we would like.
-- type Display = MVar (Map Desktop (Set Window))

-- To allow operations on separate desktops to proceed without impeding each
-- other, perhaps we can have a separate  MVar for each desktop:
type Display = Map Desktop (MVar (Set Window))

moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b =
    takeMVar ma >>= \wa ->
    takeMVar mb >>= \wb ->
    putMVar ma (Set.delete win wa) >>
    putMVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

makeWindows :: [Window] -> Set Window
makeWindows [] = Set.empty
makeWindows (w : ws) = Set.insert w (makeWindows ws)

makeWindows' :: [Window] -> IO (MVar (Set Window))
makeWindows' = newMVar . makeWindows

makeDisplay :: [Desktop] -> [[Window]] -> IO Display -> IO Display
makeDisplay [] _ display = display
makeDisplay _ [] display = display
makeDisplay (d : ds) (w : ws) display =
  makeDisplay ds ws display >>= \display' ->
  makeWindows' w >>= \windows ->
  return $ Map.insert d windows display'

makeDisplay' :: [Desktop] -> [[Window]] -> IO Display
makeDisplay' ds ws = makeDisplay ds ws (return Map.empty)

noDeadlock =
  let
    wa = Window 1
    wb = Window 2
    da = Desktop 1
    db = Desktop 2
  in
    makeDisplay' [da, db] [[wa], [wb]] >>= \display ->
    forkIO (moveWindow display wa da db)

mayDeadlock = 
  let
    wa = Window 1
    wb = Window 2
    da = Desktop 1
    db = Desktop 2
  in
    makeDisplay' [da, db] [[wa], [wb]] >>= \display ->
    moveWindow display wa da db >> print "Done here!" >>
    moveWindow display wb db da >> print "Done here, too!" >>
    threadDelay (5 * 10 ^ 6)

main :: IO ()
main = mayDeadlock
