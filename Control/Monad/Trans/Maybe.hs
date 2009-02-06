-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Maybe
-- Copyright   :  (c) 2007 Yitzak Gale, Eric Kidd
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'MaybeT' monad transformer.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Maybe (
    -- * The MaybeT monad transformer
    MaybeT(..),
    -- * Lifting other operations
    liftCallCC,
    liftListen,
    liftPass,
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM, ap)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap
 
instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((Maybe a -> m (Maybe b)) -> m (Maybe a)) ->
    m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a
liftCallCC callCC f =
    MaybeT $ callCC $ \ c -> runMaybeT (f (MaybeT . c . Just))

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m =>
    (m (Maybe a) -> m (Maybe a,w)) -> MaybeT m a -> MaybeT m (a,w)
liftListen listen = mapMaybeT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => (m (Maybe a,w -> w) -> m (Maybe a)) ->
    MaybeT m (a,w -> w) -> MaybeT m a
liftPass pass = mapMaybeT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Nothing     -> (Nothing, id)
        Just (v, f) -> (Just v, f)
