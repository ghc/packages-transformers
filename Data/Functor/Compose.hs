{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Compose
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Composition of functors.
-----------------------------------------------------------------------------

module Data.Functor.Compose (
    Compose(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

infixr 9 `Compose`

-- | Right-to-left composition of functors.
-- The composition of applicative functors is always applicative,
-- but the composition of monads is not always a monad.
newtype Compose f g a = Compose { getCompose :: f (g a) }

-- Instances of lifted Prelude classes

instance (Eq1 f, Eq1 g) => Eq1 (Compose f g) where
    eqWith eq (Compose x) (Compose y) = eqWith (eqWith eq) x y

instance (Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    compareWith comp (Compose x) (Compose y) =
        compareWith (compareWith comp) x y

instance (Read1 f, Read1 g) => Read1 (Compose f g) where
    readsPrecWith rp = readsData $
        readsUnaryWith (readsPrecWith (readsPrecWith rp)) "Compose" Compose

instance (Show1 f, Show1 g) => Show1 (Compose f g) where
    showsPrecWith sp d (Compose x) =
        showsUnaryWith (showsPrecWith (showsPrecWith sp)) "Compose" d x

-- Instances of Prelude classes

instance (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    (==) = eq1

instance (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare = compare1

instance (Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readsPrec = readsPrec1

instance (Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec = showsPrec1

-- Functor instances

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    Compose x <|> Compose y = Compose (x <|> y)
