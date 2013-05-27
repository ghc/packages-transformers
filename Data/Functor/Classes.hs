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
    -- * Liftings of Prelude classes
    Eq1(..),
    Ord1(..),
    Read1(..),
    Show1(..),
    -- * Helper functions
    readsData,
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  ) where

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

-- | Lifting of the 'Ord' class to unary type constructors.
class Eq1 f => Ord1 f where
    compare1 :: Ord a => f a -> f a -> Ordering

-- | Lifting of the 'Read' class to unary type constructors.
class Read1 f where
    readsPrec1 :: Read a => Int -> ReadS (f a)

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    showsPrec1 :: Show a => Int -> f a -> ShowS

-- Instances for Prelude type constructors

instance Eq1 Maybe where eq1 = (==)
instance Ord1 Maybe where compare1 = compare
instance Read1 Maybe where readsPrec1 = readsPrec
instance Show1 Maybe where showsPrec1 = showsPrec

instance Eq1 [] where eq1 = (==)
instance Ord1 [] where compare1 = compare
instance Read1 [] where readsPrec1 = readsPrec
instance Show1 [] where showsPrec1 = showsPrec

instance Eq a => Eq1 ((,) a) where eq1 = (==)
instance Ord a => Ord1 ((,) a) where compare1 = compare
instance Read a => Read1 ((,) a) where readsPrec1 = readsPrec
instance Show a => Show1 ((,) a) where showsPrec1 = showsPrec

instance Eq a => Eq1 (Either a) where eq1 = (==)
instance Ord a => Ord1 (Either a) where compare1 = compare
instance Read a => Read1 (Either a) where readsPrec1 = readsPrec
instance Show a => Show1 (Either a) where showsPrec1 = showsPrec

-- Building blocks

readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \r -> [res | (kw,s) <- lex r, res <- reader kw s]

readsUnary :: Read a => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

showsUnary :: Show a => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y
