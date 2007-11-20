{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadRWS class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS (
    -- * Reader-writer-state monads
    MonadRWS,
    -- * The RWS monad
    RWS,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,
    -- * The RWST monad transformer
    RWST(..),
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    MonadReader(..), asks,
    MonadState(..), modify, gets,
    MonadWriter(..), listens, censor,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader     (MonadReader(..), asks)
import Control.Monad.State      (MonadState(..), modify, gets)
import Control.Monad.Trans.Error(Error, ErrorT)
import Control.Monad.Trans.RWS.Lazy as Lazy hiding (asks, modify, gets, listens, censor)
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Writer     (MonadWriter(..), listens, censor)
import Data.Monoid

class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m)
   => MonadRWS r w s m | m -> r, m -> w, m -> s

instance (Monoid w, Monad m) => MonadRWS r w s (Lazy.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Strict.RWST r w s m)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

-- This instance needs -fallow-undecidable-instances, because
-- it does not satisfy the coverage condition
instance (Error e, MonadRWS r w s m) => MonadRWS r w s (ErrorT e m)
