{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constant
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The constant functor.
-----------------------------------------------------------------------------

module Data.Functor.Constant (
    Constant(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor (Bifunctor(..))
#endif

-- | Constant functor.
newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord)

-- These instances would be equivalent to the derived instances of the
-- newtype if the field were removed.

instance (Read a) => Read (Constant a b) where
    readsPrec = readsData $
         readsUnaryWith readsPrec "Constant" Constant

instance (Show a) => Show (Constant a b) where
    showsPrec d (Constant x) = showsUnaryWith showsPrec "Constant" d x

-- Instances of lifted Prelude classes

instance Eq2 Constant where
    liftEq2 eq _ (Constant x) (Constant y) = eq x y

instance Ord2 Constant where
    liftCompare2 comp _ (Constant x) (Constant y) = comp x y

instance Read2 Constant where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "Constant" Constant

instance Show2 Constant where
    liftShowsPrec2 sp _ _ _ d (Constant x) = showsUnaryWith sp "Constant" d x

instance (Eq a) => Eq1 (Constant a) where
    liftEq = liftEq2 (==)
instance (Ord a) => Ord1 (Constant a) where
    liftCompare = liftCompare2 compare
instance (Read a) => Read1 (Constant a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Show a) => Show1 (Constant a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure (Constant x)

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x `mappend` y)

instance (Monoid a) => Monoid (Constant a b) where
    mempty = Constant mempty
    Constant x `mappend` Constant y = Constant (x `mappend` y)

#if MIN_VERSION_base(4,8,0)
instance Bifunctor Constant where
    first f (Constant x) = Constant (f x)
    second _ (Constant x) = Constant x
#endif
