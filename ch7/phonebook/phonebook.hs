import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Data.Map as Map
import Prelude hiding (lookup)

type Name = String
type PhoneNumber = String
type PhoneBook = Map.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = 
    newMVar Map.empty >>= \m ->
    return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number =
    takeMVar m >>= \book ->
    putMVar m (Map.insert name number book)
    -- putMVar m $! Map.insert name number book

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name =
    takeMVar m >>= \book ->
    putMVar m book >>
    return (Map.lookup name book)

-- test
main = do
    s <- new
    sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
    lookup s "name999" >>= print
    lookup s "unknown" >>= print
