module WindowMan (
    moveWindow
  , makeDisplay
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, writeTVar, readTVar, newTVar)
import Control.Concurrent.STM (STM, atomically, retry)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Window = Window Int deriving (Eq, Ord, Show)
newtype Desktop = Desktop Int deriving (Eq, Ord, Show)
type Display = Map Desktop (TVar (Set Window))

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b =
    readTVar ma >>= \wa ->
    readTVar mb >>= \wb ->
    writeTVar ma (Set.delete win wa) >>
    writeTVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

makeWindows :: [Window] -> Set Window
makeWindows [] = Set.empty
makeWindows (w : ws) = Set.insert w (makeWindows ws)

makeWindowsSTM :: [Window] -> STM (TVar (Set Window))
makeWindowsSTM = newTVar . makeWindows

makeDisplaySTM :: [Desktop] -> [[Window]] -> STM Display -> STM Display
makeDisplaySTM [] _ display = display
makeDisplaySTM _ [] display = display
makeDisplaySTM (d : ds) (w : ws) display =
  makeDisplaySTM ds ws display >>= \display' ->
  makeWindowsSTM w >>= \windows ->
  return $ Map.insert d windows display'

makeDisplaySTM' :: [Desktop] -> [[Window]] -> STM Display
makeDisplaySTM' ds ws = makeDisplaySTM ds ws (return Map.empty)

makeDisplay :: [Desktop] -> [[Window]] -> IO Display
makeDisplay ds ws = atomically $ makeDisplaySTM' ds ws

type UserFocus = TVar Desktop

createFocusSTM :: Desktop -> STM UserFocus
createFocusSTM = newTVar

createFocus :: Desktop -> IO UserFocus
createFocus = atomically . createFocusSTM

changeFocusSTM :: Desktop -> UserFocus -> STM ()
changeFocusSTM desk focus = writeTVar focus desk

changeFocus :: Desktop -> UserFocus -> IO ()
changeFocus desk focus = atomically $ changeFocusSTM desk focus

getWindows :: Display -> UserFocus -> STM (Set Window)
getWindows disp focus =
  readTVar focus >>= \desktop ->
  readTVar (disp ! desktop)

render :: Set Window -> IO ()
render ws = print "Hi!"

renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus =
  atomically (getWindows disp focus) >>= \wins ->
  loop wins
  where
    loop wins =
      render wins >>
      (atomically $
        getWindows disp focus >>= \wins' ->
        if (wins == wins')
          then retry
          else return wins') >>= \next ->
      loop next

main :: IO ()
main =
  let
    wa = Window 1
    wb = Window 2
    da = Desktop 1
    db = Desktop 2
  in
    makeDisplay [da, db] [[wa], [wb]] >>= \disp ->
    createFocus da >>= \focus ->
    forkIO (renderThread disp focus) >>
    -- some problem here.
    -- whaterver what change, just two hi.
    -- moveWindow disp wa da db >>
    changeFocus db focus >>
    -- sleep 0.3s
    threadDelay (3 * 10 ^ 5) >>
    print "Done!!!"
