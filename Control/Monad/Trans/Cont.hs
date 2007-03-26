-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.Cont (
    -- * The Cont monad
    Cont,
    runCont,
    mapCont,
    withCont,
    -- * The ContT monad transformer
    ContT(..),
    mapContT,
    withContT,
    callCC,
    -- * Lifting other operations
    liftLocal,
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad

type Cont r = ContT r Identity

runCont :: Cont r a -> (a -> r) -> r
runCont m k = runIdentity (runContT m (Identity . k))

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity . f . runIdentity)

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT ((Identity .) . f . (runIdentity .))

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad, with an inner monad

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance Functor (ContT r m) where
    fmap f m = ContT $ \c -> runContT m (c . f)

instance (Monad m) => Monad (ContT r m) where
    return a = ContT ($ a)
    m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO

callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

-- | @'liftLocal' ask local@ yields a @local@ function for @'ContT' r m@.
liftLocal :: Monad m => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> ContT r m a -> ContT r m a
liftLocal ask local f m = ContT $ \c -> do
    r <- ask
    local f (runContT m (local (const r) . c))
