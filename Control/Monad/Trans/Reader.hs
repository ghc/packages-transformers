-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Reader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the MonadReader class
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Reader (
    -- * The Reader monad
    Reader,
    reader,
    runReader,
    mapReader,
    withReader,
    -- * The ReaderT monad transformer
    ReaderT(..),
    mapReaderT,
    withReaderT,
    -- * Reader operations
    ask,
    local,
    asks,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    ) where

import Control.Monad.Identity
import Control.Monad.Trans

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Instances ()

-- | The parameterizable reader monad.
--
-- The 'return' function creates a @Reader@ that ignores the environment,
-- and produces the given value.
--
-- The binding operator @>>=@ produces a @Reader@ that uses the
-- environment to extract the value its left-hand side, and then applies
-- the bound function to that value in the same environment.
type Reader r = ReaderT r Identity

reader :: (r -> a) -> Reader r a
reader f = ReaderT (Identity . f)

-- | Runs @Reader@ and extracts the final value from it.
runReader :: Reader r a		-- ^ A @Reader@ to run.
    -> r			-- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

-- | A more general version of 'local'.
withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader = withReaderT

-- | The reader monad transformer.
-- Can be used to add environment reading functionality to other monads.
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Functor m) => Functor (ReaderT r m) where
    fmap f m = ReaderT (fmap f . runReaderT m)

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = ReaderT $ \_ -> mzero
    m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

local :: (Monad m) => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f m = ReaderT $ \r -> runReaderT m (f r)

asks :: (Monad m) => (r -> a) -> ReaderT r m a
asks f = do
    r <- ask
    return (f r)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((a -> m b) -> m a) -> m a) ->
    ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a
liftCallCC callCC f = ReaderT $ \r ->
    callCC $ \c ->
    runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m a -> (e -> m a) -> m a) ->
    ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
liftCatch f m h = ReaderT $ \r -> f (runReaderT m r) (\e -> runReaderT (h e) r)
