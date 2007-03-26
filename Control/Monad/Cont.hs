-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Control.Monad.Cont (
    -- * MonadCont class
    MonadCont(..),
    -- * The Cont monad
    Cont,
    runCont,
    mapCont,
    withCont,
    -- * The ContT monad transformer
    ContT(..),
    mapContT,
    withContT,
    module Control.Monad,
    module Control.Monad.Trans,
  ) where

import Control.Monad
import Control.Monad.Trans.Cont hiding (callCC)
import qualified Control.Monad.Trans.Cont as ContT
import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.List as List
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.RWS.Lazy as LazyRWS
import Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Trans.State.Strict as StrictState
import Control.Monad.Trans.Writer.Lazy as LazyWriter
import Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans
import Data.Monoid

class (Monad m) => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance (Monad m) => MonadCont (ContT r m) where
    callCC = ContT.callCC

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Error e, MonadCont m) => MonadCont (ErrorT e m) where
    callCC = Error.liftCallCC callCC

instance (MonadCont m) => MonadCont (ListT m) where
    callCC = List.liftCallCC callCC

instance (MonadCont m) => MonadCont (ReaderT r m) where
    callCC = Reader.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyRWS.RWST r w s m) where
    callCC = LazyRWS.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictRWS.RWST r w s m) where
    callCC = StrictRWS.liftCallCC callCC

instance (MonadCont m) => MonadCont (LazyState.StateT s m) where
    callCC = LazyState.liftCallCC callCC

instance (MonadCont m) => MonadCont (StrictState.StateT s m) where
    callCC = StrictState.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyWriter.WriterT w m) where
    callCC = LazyWriter.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictWriter.WriterT w m) where
    callCC = StrictWriter.liftCallCC callCC
