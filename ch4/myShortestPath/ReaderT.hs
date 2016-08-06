{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module ReaderT where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Interface

instance Functor (Reader env) where
    fmap = liftM

instance Applicative (Reader env) where
    pure = return
    (<*>) = ap

instance Monad (Reader env) where
    return x = Reader (\_ -> x)
    f >>= g = Reader $ \x -> runReader (g (runReader f x)) x

instance MonadReader env (Reader env) where
    ask = Reader $ \x -> x
    local f g = Reader $ \x -> runReader g (f x)

instance (Monad m) => Functor (ReaderT env m) where
    fmap = liftM

instance (Monad m) => Applicative (ReaderT env m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (ReaderT env m) where
    return x = ReaderT (\_ -> return x)
    f >>= g = ReaderT $ \x -> runReaderT f x >>= \a -> runReaderT (g a) x

instance (Monad m) => MonadReader env (ReaderT env m) where
    ask = ReaderT $ \x -> return x
    local f g = ReaderT $ \x -> runReaderT g (f x)

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (const m)

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
