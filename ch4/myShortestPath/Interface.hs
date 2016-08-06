{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Interface where

newtype Reader env a = Reader {
    runReader :: env -> a
}

newtype ReaderT env m a = ReaderT {
    runReaderT :: env -> m a
}

class (Monad m) => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a
