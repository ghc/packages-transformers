{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Reader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadReader class
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Reader (
    -- * MonadReader class
    MonadReader(..),
    asks,
    -- * The Reader monad
    Reader,
    runReader,
    mapReader,
    withReader,
    -- * The ReaderT monad transformer
    ReaderT(..),
    mapReaderT,
    withReaderT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader hiding (ask, local, asks)
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans
import Data.Monoid

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a

-- This allows you to provide a projection function.

asks :: (MonadReader r m) => (r -> a) -> m a
asks f = do
    r <- ask
    return (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask = ReaderT.ask
    local = ReaderT.local

instance (Monad m, Monoid w) => MonadReader r (LazyRWS.RWST r w s m) where
    ask = LazyRWS.ask
    local = LazyRWS.local

instance (Monad m, Monoid w) => MonadReader r (StrictRWS.RWST r w s m) where
    ask = StrictRWS.ask
    local = StrictRWS.local

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

-- Needs -fallow-undecidable-instances
instance (MonadReader r' m) => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local

-- Needs -fallow-undecidable-instances
instance (Error e, MonadReader r m) => MonadReader r (ErrorT e m) where
    ask   = lift ask
    local = mapErrorT . local

-- Needs -fallow-undecidable-instances
instance (MonadReader s m) => MonadReader s (ListT m) where
    ask   = lift ask
    local = mapListT . local

-- Needs -fallow-undecidable-instances
instance (MonadReader r m) => MonadReader r (Lazy.StateT s m) where
    ask   = lift ask
    local = Lazy.mapStateT . local

-- Needs -fallow-undecidable-instances
instance (MonadReader r m) => MonadReader r (Strict.StateT s m) where
    ask   = lift ask
    local = Strict.mapStateT . local

-- This instance needs -fallow-undecidable-instances, because
-- it does not satisfy the coverage condition
instance (Monoid w, MonadReader r m) => MonadReader r (Lazy.WriterT w m) where
    ask   = lift ask
    local = Lazy.mapWriterT . local

-- This instance needs -fallow-undecidable-instances, because
-- it does not satisfy the coverage condition
instance (Monoid w, MonadReader r m) => MonadReader r (Strict.WriterT w m) where
    ask   = lift ask
    local = Strict.mapWriterT . local
