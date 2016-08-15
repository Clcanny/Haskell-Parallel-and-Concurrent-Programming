import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, writeTVar, readTVar, newTVar)
import Control.Concurrent.STM (STM, atomically)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Window = Window Int deriving (Eq, Ord, Show)
newtype Desktop = Desktop Int deriving (Eq, Ord, Show)
type Display = Map Desktop (TVar (Set Window))

-- readTVar :: TVar a -> STM a
-- writeTVar :: TVar a -> a -> STM ()

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b =
    readTVar ma >>= \wa ->
    readTVar mb >>= \wb ->
    writeTVar ma (Set.delete win wa) >>
    writeTVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

-- the sequence of operations inside  atomically happens indivisibly as far
-- as the rest of the program is concerned, so the problem we encountered
-- earlier that required taking MVars in the correct order does not occur. 
-- 如果moveWindowsSTM保持完整性，不会暴露任何中间状态，那么我们自然不需要担心死锁的问题。
-- 哲学家进餐问题？
moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

-- This demonstrates the composability of STM operations: any operation of type
-- STM a can be composed with others to form a larger atomic transaction. For this
-- reason, STM operations are usually provided without the  atomically wrapper so that
-- clients can compose them as necessary before finally wrapping the entire operation in
-- atomically.
-- 另外，可以推理出：
-- STM内的操作具有“可逆“/“可取消”的性质。
swapWindowsSTM :: Display -> Window -> Desktop -> Window -> Desktop -> STM()
swapWindowsSTM disp w a v b =
  moveWindowSTM disp w a b >>
  moveWindowSTM disp v b a

-- Why is  STM a different monad from  IO ? The STM implementation relies
-- on being able to roll back the effects of a transaction in the event of a
-- conflict with another transaction (and for other reasons, as we shall
-- see shortly). A transaction can be rolled back only if we can track
-- exactly what effects it has, and this would not be possible if arbitrary
-- I/O were allowed inside a transaction—we might have performed some
-- I/O that cannot be undone, like making a noise or launching some
-- missiles. For this reason, the  STM monad permits only side effects on
-- TVar s, and the STM implementation tracks these effects to ensure the
-- correct transaction semantics. 

swapWindows :: Display -> Window -> Desktop -> Window -> Desktop -> IO ()
swapWindows disp w a v b = 
  atomically (swapWindowsSTM disp w a v b) >>
  print "Finish!"

makeWindows :: [Window] -> Set Window
makeWindows [] = Set.empty
makeWindows (w : ws) = Set.insert w (makeWindows ws)

-- newTVar :: a -> STM (TVar a)

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

noDeadlock = 
  let
    wa = Window 1
    wb = Window 2
    da = Desktop 1
    db = Desktop 2
  in
    makeDisplay [da, db] [[wa], [wb]] >>= \display ->
    swapWindows display wa db wb da >>
    threadDelay (5 * 10 ^ 6)

-- main :: IO ()
-- main = mayDeadlock
