{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Liftings of the Prelude classes 'Eq', 'Ord', 'Read' and 'Show' to
-- unary and binary type constructors.
--
-- These classes are needed to express the constraints on arguments of
-- transformers in portable Haskell.  Thus for a new transformer @T@,
-- one might write instances like
--
-- > instance (Eq1 f) => Eq1 (T f) where ...
-- > instance (Ord1 f) => Ord1 (T f) where ...
-- > instance (Read1 f) => Read1 (T f) where ...
-- > instance (Show1 f) => Show1 (T f) where ...
--
-- If these instances can be defined, defining instances of the base
-- classes is mechanical:
--
-- > instance (Eq1 f, Eq a) => Eq (T f a) where (==) = eq1
-- > instance (Ord1 f, Ord a) => Ord (T f a) where compare = compare1
-- > instance (Read1 f, Read a) => Read (T f a) where readsPrec = readsPrec1
-- > instance (Show1 f, Show a) => Show (T f a) where showsPrec = showsPrec1
--
-----------------------------------------------------------------------------

module Data.Functor.Classes (
    -- * Liftings of Prelude classes
    -- ** For unary constructors
    Eq1(..), eq1,
    Ord1(..), compare1,
    Read1(..), readsPrec1,
    Show1(..), showsPrec1,
    -- ** For binary constructors
    Eq2(..), eq2,
    Ord2(..), compare2,
    Read2(..), readsPrec2,
    Show2(..), showsPrec2,
    -- * Helper functions
    -- $example
    readsData,
    readsUnaryWith,
    readsBinaryWith,
    showsUnaryWith,
    showsBinaryWith,
    -- ** Obsolete helpers
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  ) where

import Control.Applicative (Const(Const))
import Data.Functor.Identity (Identity(Identity))
import Data.Monoid (mappend)

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    -- | Lift an equality test through the type constructor.
    --
    -- The function will usually be applied to an equality function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    eqWith :: (a -> b -> Bool) -> f a -> f b -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = eqWith (==)

-- | Lifting of the 'Ord' class to unary type constructors.
class (Eq1 f) => Ord1 f where
    -- | Lift a 'compare' function through the type constructor.
    --
    -- The function will usually be applied to a comparison function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    compareWith :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = compareWith compare

-- | Lifting of the 'Read' class to unary type constructors.
class Read1 f where
    -- | Lift a 'readsPrec' function through the type constructor.
    readsPrecWith :: (Int -> ReadS a) -> Int -> ReadS (f a)

-- | Lift the standard 'readsPrec' function through the type constructor.
readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = readsPrecWith readsPrec

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    -- | Lift a 'showsPrec' function through the type constructor.
    showsPrecWith :: (Int -> a -> ShowS) -> Int -> f a -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = showsPrecWith showsPrec

-- | Lifting of the 'Eq' class to binary type constructors.
class Eq2 f where
    -- | Lift equality tests through the type constructor.
    --
    -- The function will usually be applied to equality functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    eqWith2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = eqWith2 (==) (==)

-- | Lifting of the 'Ord' class to binary type constructors.
class (Eq2 f) => Ord2 f where
    -- | Lift 'compare' functions through the type constructor.
    --
    -- The function will usually be applied to comparison functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    compareWith2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) ->
        f a c -> f b d -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = compareWith2 compare compare

-- | Lifting of the 'Read' class to binary type constructors.
class Read2 f where
    -- | Lift 'readsPrec' functions through the type constructor.
    readsPrecWith2 :: (Int -> ReadS a) -> (Int -> ReadS b) ->
        Int -> ReadS (f a b)

-- | Lift the standard 'readsPrec' function through the type constructor.
readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = readsPrecWith2 readsPrec readsPrec

-- | Lifting of the 'Show' class to binary type constructors.
class Show2 f where
    -- | Lift 'showsPrec' functions through the type constructor.
    showsPrecWith2 :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
        Int -> f a b -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = showsPrecWith2 showsPrec showsPrec

-- Instances for Prelude type constructors

instance Eq1 Maybe where
    eqWith _ Nothing Nothing = True
    eqWith _ Nothing (Just _) = False
    eqWith _ (Just _) Nothing = False
    eqWith eq (Just x) (Just y) = eq x y

instance Ord1 Maybe where
    compareWith _ Nothing Nothing = EQ
    compareWith _ Nothing (Just _) = LT
    compareWith _ (Just _) Nothing = GT
    compareWith comp (Just x) (Just y) = comp x y

instance Read1 Maybe where
    readsPrecWith rp d =
         readParen False (\ r -> [(Nothing,s) | ("Nothing",s) <- lex r])
         `mappend`
         readsData (readsUnaryWith rp "Just" Just) d

instance Show1 Maybe where
    showsPrecWith _ _ Nothing = showString "Nothing"
    showsPrecWith sp d (Just x) = showsUnaryWith sp "Just" d x

instance Eq1 [] where
    eqWith _ [] [] = True
    eqWith _ [] (_:_) = False
    eqWith _ (_:_) [] = False
    eqWith eq (x:xs) (y:ys) = eq x y && eqWith eq xs ys

instance Ord1 [] where
    compareWith _ [] [] = EQ
    compareWith _ [] (_:_) = LT
    compareWith _ (_:_) [] = GT
    compareWith comp (x:xs) (y:ys) = comp x y `mappend` compareWith comp xs ys

instance Read1 [] where
    readsPrecWith rp _ = readParen False $ \ r ->
        [pr | ("[",s)  <- lex r, pr <- readl s]
      where
        readl s = [([],t) | ("]",t) <- lex s] ++
            [(x:xs,u) | (x,t) <- rp 0 s, (xs,u) <- readl' t]
        readl' s = [([],t) | ("]",t) <- lex s] ++
            [(x:xs,v) | (",",t) <- lex s, (x,u) <- rp 0 t, (xs,v) <- readl' u]


instance Show1 [] where
    showsPrecWith _ _ [] = showString "[]"
    showsPrecWith sp _ (x:xs) = showChar '[' . sp 0 x . showl xs
      where
        showl []     = showChar ']'
        showl (y:ys) = showChar ',' . sp 0 y . showl ys

instance Eq2 (,) where
    eqWith2 e1 e2 (x1, y1) (x2, y2) = e1 x1 x2 && e2 y1 y2

instance Ord2 (,) where
    compareWith2 comp1 comp2 (x1, y1) (x2, y2) =
        comp1 x1 x2 `mappend` comp2 y1 y2

instance Read2 (,) where
    readsPrecWith2 rp1 rp2 _ = readParen False $ \ r ->
        [((x,y), w) | ("(",s) <- lex r,
                      (x,t)   <- rp1 0 s,
                      (",",u) <- lex t,
                      (y,v)   <- rp2 0 u,
                      (")",w) <- lex v]

instance Show2 (,) where
    showsPrecWith2 sp1 sp2 _ (x, y) =
        showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

instance (Eq a) => Eq1 ((,) a) where
    eqWith = eqWith2 (==)

instance (Ord a) => Ord1 ((,) a) where
    compareWith = compareWith2 compare

instance (Read a) => Read1 ((,) a) where
    readsPrecWith = readsPrecWith2 readsPrec

instance (Show a) => Show1 ((,) a) where
    showsPrecWith = showsPrecWith2 showsPrec

instance Eq2 Either where
    eqWith2 e1 _ (Left x) (Left y) = e1 x y
    eqWith2 _ _ (Left _) (Right _) = False
    eqWith2 _ _ (Right _) (Left _) = False
    eqWith2 _ e2 (Right x) (Right y) = e2 x y

instance Ord2 Either where
    compareWith2 comp1 _ (Left x) (Left y) = comp1 x y
    compareWith2 _ _ (Left _) (Right _) = LT
    compareWith2 _ _ (Right _) (Left _) = GT
    compareWith2 _ comp2 (Right x) (Right y) = comp2 x y

instance Read2 Either where
    readsPrecWith2 rp1 rp2 = readsData $
         readsUnaryWith rp1 "Left" Left `mappend`
         readsUnaryWith rp2 "Right" Right

instance Show2 Either where
    showsPrecWith2 sp1 _ d (Left x) = showsUnaryWith sp1 "Left" d x
    showsPrecWith2 _ sp2 d (Right x) = showsUnaryWith sp2 "Right" d x

instance (Eq a) => Eq1 (Either a) where
    eqWith = eqWith2 (==)

instance (Ord a) => Ord1 (Either a) where
    compareWith = compareWith2 compare

instance (Read a) => Read1 (Either a) where
    readsPrecWith = readsPrecWith2 readsPrec

instance (Show a) => Show1 (Either a) where
    showsPrecWith = showsPrecWith2 showsPrec

-- Instances for other functors defined in the base package

instance Eq1 Identity where
    eqWith eq (Identity x) (Identity y) = eq x y

instance Ord1 Identity where
    compareWith comp (Identity x) (Identity y) = comp x y

instance Read1 Identity where
    readsPrecWith rp = readsData $
         readsUnaryWith rp "Identity" Identity

instance Show1 Identity where
    showsPrecWith sp d (Identity x) = showsUnaryWith sp "Identity" d x

instance Eq2 Const where
    eqWith2 eq _ (Const x) (Const y) = eq x y

instance Ord2 Const where
    compareWith2 comp _ (Const x) (Const y) = comp x y

instance Read2 Const where
    readsPrecWith2 rp _ = readsData $
         readsUnaryWith rp "Const" Const

instance Show2 Const where
    showsPrecWith2 sp _ d (Const x) = showsUnaryWith sp "Const" d x

instance (Eq a) => Eq1 (Const a) where
    eqWith = eqWith2 (==)
instance (Ord a) => Ord1 (Const a) where
    compareWith = compareWith2 compare
instance (Read a) => Read1 (Const a) where
    readsPrecWith = readsPrecWith2 readsPrec
instance (Show a) => Show1 (Const a) where
    showsPrecWith = showsPrecWith2 showsPrec

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readsUnary', 'readsUnary1' and 'readsBinary1', and combined with
-- @mappend@ from the @Monoid@ class.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \ r -> [res | (kw,s) <- lex r, res <- reader kw s]

-- | @'readsUnaryWith' rp n c n'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith rp name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- rp 11 s]

-- | @'readsBinaryWith' rp1 rp2 n c n'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) ->
    String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith rp1 rp2 name cons kw s =
    [(cons x y,u) | kw == name, (x,t) <- rp1 11 s, (y,u) <- rp2 11 t]

-- | @'showsUnaryWith' sp n d x@ produces the string representation of a
-- unary data constructor with name @n@ and argument @x@, in precedence
-- context @d@.
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith sp name d x = showParen (d > 10) $
    showString name . showChar ' ' . sp 11 x

-- | @'showsBinaryWith' sp1 sp2 n d x y@ produces the string
-- representation of a binary data constructor with name @n@ and arguments
-- @x@ and @y@, in precedence context @d@.
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
    String -> Int -> a -> b -> ShowS
showsBinaryWith sp1 sp2 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y

-- Obsolete building blocks

-- | @'readsUnary' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec'.
{-# DEPRECATED readsUnary "Use readsUnaryWith to define readsPrecWith" #-}
readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

-- | @'readsUnary1' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec1'.
{-# DEPRECATED readsUnary1 "Use readsUnaryWith to define readsPrecWith" #-}
readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

-- | @'readsBinary1' n c n'@ matches the name of a binary data constructor
-- and then parses its arguments using 'readsPrec1'.
{-# DEPRECATED readsBinary1 "Use readsBinaryWith to define readsPrecWith" #-}
readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

-- | @'showsUnary' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
{-# DEPRECATED showsUnary "Use showsUnaryWith to define showsPrecWith" #-}
showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

-- | @'showsUnary1' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
{-# DEPRECATED showsUnary1 "Use showsUnaryWith to define showsPrecWith" #-}
showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

-- | @'showsBinary1' n d x y@ produces the string representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @d@.
{-# DEPRECATED showsBinary1 "Use showsBinaryWith to define showsPrecWith" #-}
showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y

{- $example
These functions can be used to assemble 'Read' and 'Show' instances for
new algebraic types.  For example, given the definition

> data T f a = Zero a | One (f a) | Two a (f a)

a standard 'Read1' instance may be defined as

> instance (Read1 f) => Read1 (T f) where
>     readsPrecWith rp = readsData $
>         readsUnaryWith rp "Zero" Zero `mappend`
>         readsUnaryWith (readsPrecWith rp) "One" One `mappend`
>         readsBinaryWith rp (readsPrecWith rp) "Two" Two

and the corresponding 'Show1' instance as

> instance (Show1 f) => Show1 (T f) where
>     showsPrecWith sp d (Zero x) =
>         showsUnaryWith sp "Zero" d x
>     showsPrecWith sp d (One x) =
>         showsUnaryWith (showsPrecWith sp) "One" d x
>     showsPrecWith sp d (Two x y) =
>         showsBinaryWith sp (showsPrecWith sp) "Two" d x y

-}
