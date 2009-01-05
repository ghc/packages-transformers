-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Lazy writer monads.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer.Lazy (
    -- * The Writer monad
    Writer,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    -- * Writer operations
    tell,
    listen,
    pass,
    listens,
    censor,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Monoid

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad

type Writer w = WriterT w Identity

writer :: (a, w) -> Writer w a
writer = WriterT . Identity

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad, with an inner monad

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
    ~(_, w) <- runWriterT m
    return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Monad m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ liftM $ \ ~(a, w) -> (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO

tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)

listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w)

pass :: (Monoid w, Monad m) => WriterT w m (a, w -> w) -> WriterT w m a
pass m = WriterT $ do
    ~((a, f), w) <- runWriterT m
    return (a, f w)

listens :: (Monoid w, Monad m) => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) => ((((a,w) -> m (b,w)) -> m (a,w)) -> m (a,w)) ->
    ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a
liftCallCC callCC f = WriterT $
    callCC $ \c ->
    runWriterT (f (\a -> WriterT $ c (a, mempty)))

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a,w) -> (e -> m (a,w)) -> m (a,w)) ->
    WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
liftCatch catchError m h =
    WriterT $ runWriterT m `catchError` \e -> runWriterT (h e)
