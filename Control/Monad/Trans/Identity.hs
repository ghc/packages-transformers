-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Identity
-- Copyright   :  (c) 2007 Magnus Therning
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the identity monad transformer.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Identity (
    -- * The identity monad transformer
    IdentityT(..),
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    liftListen,
    liftLocal,
    liftPass,
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM, ap)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))

newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance (Functor m) => Functor (IdentityT m) where
    fmap f = IdentityT . fmap f . runIdentityT

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    f <*> v = IdentityT (runIdentityT f <*> runIdentityT v)
 
instance (Alternative m) => Alternative (IdentityT m) where
    empty = IdentityT empty
    f <|> v = IdentityT (runIdentityT f <|> runIdentityT v)

instance (Monad m) => Monad (IdentityT m) where
    return = IdentityT . return
    m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m
    fail msg = IdentityT $ fail msg
 
instance (MonadPlus m) => MonadPlus (IdentityT m) where
    mzero = IdentityT mzero
    f `mplus` v = IdentityT (runIdentityT f `mplus` runIdentityT v)

instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance MonadTrans IdentityT where
    lift = IdentityT

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((a -> m b) -> m a) ->
    m a) -> ((a -> IdentityT m b) -> IdentityT m a) -> IdentityT m a
liftCallCC callCC f =
    IdentityT $ callCC $ \ c -> runIdentityT (f (IdentityT . c))

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m a -> (e -> m a) -> m a) ->
    IdentityT m a -> (e -> IdentityT m a) -> IdentityT m a
liftCatch f m h = IdentityT $ f (runIdentityT m) (runIdentityT . h)

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m =>
    (m a -> m (a,w)) -> IdentityT m a -> IdentityT m (a,w)
liftListen listen = IdentityT . listen . runIdentityT

-- | Lift a @local@ operation to the new monad.
liftLocal :: Monad m => ((r -> r) -> m a -> m a) ->
    (r -> r) -> IdentityT m a -> IdentityT m a
liftLocal local f = IdentityT . local f . runIdentityT

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => (m (a,w -> w) -> m a) ->
    IdentityT m (a,w -> w) -> IdentityT m a
liftPass pass = IdentityT . pass . runIdentityT
