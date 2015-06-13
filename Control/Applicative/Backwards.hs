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
-- Module      :  Control.Applicative.Backwards
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors with an 'Applicative' instance that performs actions
-- in the reverse order.
-----------------------------------------------------------------------------

module Control.Applicative.Backwards (
    Backwards(..),
  ) where

import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable

-- | The same functor, but with an 'Applicative' instance that performs
-- actions in the reverse order.
newtype Backwards f a = Backwards { forwards :: f a }

instance (Eq1 f) => Eq1 (Backwards f) where
    eqWith eq (Backwards x) (Backwards y) = eqWith eq x y

instance (Ord1 f) => Ord1 (Backwards f) where
    compareWith comp (Backwards x) (Backwards y) = compareWith comp x y

instance (Read1 f) => Read1 (Backwards f) where
    readsPrecWith rp = readsData $
        readsUnaryWith (readsPrecWith rp) "Backwards" Backwards

instance (Show1 f) => Show1 (Backwards f) where
    showsPrecWith sp d (Backwards x) =
        showsUnaryWith (showsPrecWith sp) "Backwards" d x

instance (Eq1 f, Eq a) => Eq (Backwards f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Backwards f a) where compare = compare1
instance (Read1 f, Read a) => Read (Backwards f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Backwards f a) where showsPrec = showsPrec1

-- | Derived instance.
instance (Functor f) => Functor (Backwards f) where
    fmap f (Backwards a) = Backwards (fmap f a)

-- | Apply @f@-actions in the reverse order.
instance (Applicative f) => Applicative (Backwards f) where
    pure a = Backwards (pure a)
    Backwards f <*> Backwards a = Backwards (a <**> f)

-- | Try alternatives in the same order as @f@.
instance (Alternative f) => Alternative (Backwards f) where
    empty = Backwards empty
    Backwards x <|> Backwards y = Backwards (x <|> y)

-- | Derived instance.
instance (Foldable f) => Foldable (Backwards f) where
    foldMap f (Backwards t) = foldMap f t
    foldr f z (Backwards t) = foldr f z t
    foldl f z (Backwards t) = foldl f z t
    foldr1 f (Backwards t) = foldr1 f t
    foldl1 f (Backwards t) = foldl1 f t

-- | Derived instance.
instance (Traversable f) => Traversable (Backwards f) where
    traverse f (Backwards t) = fmap Backwards (traverse f t)
    sequenceA (Backwards t) = fmap Backwards (sequenceA t)
