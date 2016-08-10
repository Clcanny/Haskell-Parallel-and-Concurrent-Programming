{-# LANGUAGE DeriveDataTypeable #-}

-- throw :: Exception e => e -> a
-- class (Typeable e, Show e) => Exception e ...

import Control.Exception
import Data.Typeable

data MyException = MyException deriving (Show, Typeable)

instance Exception MyException

-- throw :: Exception e => e -> a
a = throw MyException

-- action `catch` \e -> handler
b :: IO ()
b = throw MyException `catch` \e -> print (e :: MyException)

c :: IO ()
c = throw (ErrorCall "oops") `catch` \e -> print (e :: MyException)

d :: IO ()
d = throw (ErrorCall "oops") `catch` \e -> print (e :: SomeException)

e :: IO ()
e = throw MyException `catch` \e -> print (e :: SomeException)

-- try :: Exception e => IO a -> IO (Either e a)
f = try (readFile "nonexistent") :: IO (Either IOException String)
g = try (readFile "exception.hs") :: IO (Either IOException String)
h = try (throw MyException) :: IO (Either MyException a)

-- handle :: Exception e => (e -> IO a) -> IO a -> IO a
-- change the first argument and the second argument of catch
-- handle (\e -> ...) $ do ...

-- onException :: IO a -> IO b -> IO a
-- perform some operation if an exception is raised and then re-throw the exception
onException' :: IO a -> IO b -> IO a
onException' io what = io `catch` \e ->
    what >>
    -- throwIO :: Exception e => e -> IO a
    -- throw :: Exception e => e -> a
    -- throwIO guarantees strict ordering with respect to other IO operations.
    -- throwIO make sure the exceptions is catched in the IO monad, and throw guarantees this too.
    throwIO (e :: SomeException)

i = onException (throw MyException) (print "meet a exception")
j = onException' (throw MyException) (print "meet a exception")
k = i `catch` \e -> print (e :: MyException)
l = j `catch` \e -> print (e :: MyException)

-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- bracket guarantees resources always be deallocated, even if meet an exception.
bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' before after during =
    before >>= \a ->
    during a `onException` after a >>= \c ->
    after a >>
    return c

-- finally is a special case of bracket.
finally' :: IO a -> IO b -> IO a
finally' io after =
    io `onException` after >>= \a ->
    after >>
    return a
