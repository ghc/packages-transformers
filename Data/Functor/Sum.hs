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
-- Module      :  Data.Functor.Sum
-- Copyright   :  (c) Ross Paterson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Sums, lifted to functors.
-----------------------------------------------------------------------------

module Data.Functor.Sum (
    Sum(..),
  ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Classes
import Data.Monoid (mappend)
import Data.Traversable (Traversable(traverse))

-- | Lifted sum of functors.
data Sum f g a = InL (f a) | InR (g a)

instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
    eqWith eq (InL x1) (InL x2) = eqWith eq x1 x2
    eqWith _ (InL _) (InR _) = False
    eqWith _ (InR _) (InL _) = False
    eqWith eq (InR y1) (InR y2) = eqWith eq y1 y2

instance (Ord1 f, Ord1 g) => Ord1 (Sum f g) where
    compareWith comp (InL x1) (InL x2) = compareWith comp x1 x2
    compareWith _ (InL _) (InR _) = LT
    compareWith _ (InR _) (InL _) = GT
    compareWith comp (InR y1) (InR y2) = compareWith comp y1 y2

instance (Read1 f, Read1 g) => Read1 (Sum f g) where
    readsPrecWith rp = readsData $
        readsUnaryWith (readsPrecWith rp) "InL" InL `mappend`
        readsUnaryWith (readsPrecWith rp) "InR" InR

instance (Show1 f, Show1 g) => Show1 (Sum f g) where
    showsPrecWith sp d (InL x) = showsUnaryWith (showsPrecWith sp) "InL" d x
    showsPrecWith sp d (InR y) = showsUnaryWith (showsPrecWith sp) "InR" d y

instance (Eq1 f, Eq1 g, Eq a) => Eq (Sum f g a) where
    (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (Sum f g a) where
    compare = compare1
instance (Read1 f, Read1 g, Read a) => Read (Sum f g a) where
    readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show (Sum f g a) where
    showsPrec = showsPrec1

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap f (InL x) = InL (fmap f x)
    fmap f (InR y) = InR (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Sum f g) where
    foldMap f (InL x) = foldMap f x
    foldMap f (InR y) = foldMap f y

instance (Traversable f, Traversable g) => Traversable (Sum f g) where
    traverse f (InL x) = InL <$> traverse f x
    traverse f (InR y) = InR <$> traverse f y
