-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Prelude classes, lifted to unary type constructors.

module Data.Functor.Classes (
    Eq1(..),
    Ord1(..),
    Show1(..),
  ) where

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

-- | Lifting of the 'Ord' class to unary type constructors.
class Eq1 f => Ord1 f where
    compare1 :: Ord a => f a -> f a -> Ordering

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    showsPrec1 :: Show a => Int -> f a -> ShowS

-- Instances for Prelude type constructors

instance Eq1 Maybe where eq1 = (==)
instance Ord1 Maybe where compare1 = compare
instance Show1 Maybe where showsPrec1 = showsPrec

instance Eq1 [] where eq1 = (==)
instance Ord1 [] where compare1 = compare
instance Show1 [] where showsPrec1 = showsPrec

instance Eq a => Eq1 ((,) a) where eq1 = (==)
instance Ord a => Ord1 ((,) a) where compare1 = compare
instance Show a => Show1 ((,) a) where showsPrec1 = showsPrec

instance Eq a => Eq1 (Either a) where eq1 = (==)
instance Ord a => Ord1 (Either a) where compare1 = compare
instance Show a => Show1 (Either a) where showsPrec1 = showsPrec
