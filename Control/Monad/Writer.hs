{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer (
    -- * MonadWriter class
    MonadWriter(..),
    listens,
    censor,
    -- * The Writer monad
    Writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (
        RWST, tell, listen, pass)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (
        RWST, tell, listen, pass)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (
        WriterT, tell, listen, pass)
import qualified Control.Monad.Trans.Writer.Strict as Strict (
        WriterT, tell, listen, pass)
import Control.Monad.Trans.Writer (
        Writer, runWriter, execWriter, mapWriter,
        WriterT(..), execWriterT, mapWriterT)
import Control.Monad.Trans
import Data.Monoid

-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement).
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    tell   :: w -> m ()
    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

instance (Monoid w, Monad m) => MonadWriter w (Lazy.WriterT w m) where
    tell   = Lazy.tell
    listen = Lazy.listen
    pass   = Lazy.pass

instance (Monoid w, Monad m) => MonadWriter w (Strict.WriterT w m) where
    tell   = Strict.tell
    listen = Strict.listen
    pass   = Strict.pass

instance (Monoid w, Monad m) => MonadWriter w (LazyRWS.RWST r w s m) where
    tell   = LazyRWS.tell
    listen = LazyRWS.listen
    pass   = LazyRWS.pass

instance (Monoid w, Monad m) => MonadWriter w (StrictRWS.RWST r w s m) where
    tell   = StrictRWS.tell
    listen = StrictRWS.listen
    pass   = StrictRWS.pass

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need -fallow-undecidable-instances

instance (Error e, MonadWriter w m) => MonadWriter w (ErrorT e m) where
    tell   = lift . tell
    listen = Error.liftListen listen
    pass   = Error.liftPass pass

instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
    tell   = lift . tell
    listen = mapReaderT listen
    pass   = mapReaderT pass

instance (MonadWriter w m) => MonadWriter w (Lazy.StateT s m) where
    tell   = lift . tell
    listen = Lazy.liftListen listen
    pass   = Lazy.liftPass pass

instance (MonadWriter w m) => MonadWriter w (Strict.StateT s m) where
    tell   = lift . tell
    listen = Strict.liftListen listen
    pass   = Strict.liftPass pass
