-- |
-- Module      :  Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.

module Data.Functor.Identity (
    Identity(..),
    runIdentity,
  ) where

import Data.Functor.Classes

import Control.Applicative
import Control.Monad.Fix
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

-- | Identity functor and monad.
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

-- | Inverse of 'Identity'.
runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Eq1 Identity where eq1 = (==)
instance Ord1 Identity where compare1 = compare
instance Show1 Identity where showsPrec1 = showsPrec

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f = Identity (fix (runIdentity . f))
