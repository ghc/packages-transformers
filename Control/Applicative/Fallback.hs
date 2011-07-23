-- |
-- Module      :  Control.Applicative.Fallback
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Applicative functor that collects errors.

module Control.Applicative.Fallback (
    Fallback(..), fallback, Errors, failure
  ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Constant
import Data.Monoid (Monoid(mappend))
import Data.Traversable (Traversable(traverse))

-- | Applicative functor for computations that may fail, falling back into
-- a given appplicative functor.
-- A sequence of computations fails if any of its components do, but
-- unlike monads made with 'ErrorT' from "Control.Monad.Trans.Error",
-- these computations continue after an error, collecting all the failures.

data Fallback f a = Success a | Failure (f a)

instance (Functor f) => Functor (Fallback f) where
    fmap f (Success x) = Success (f x)
    fmap f (Failure y) = Failure (fmap f y)

instance (Foldable f) => Foldable (Fallback f) where
    foldMap f (Success x) = f x
    foldMap f (Failure y) = foldMap f y

instance (Traversable f) => Traversable (Fallback f) where
    traverse f (Success x) = Success <$> f x
    traverse f (Failure y) = Failure <$> traverse f y

-- | Fail if any step fails, but continue to collect errors from later steps.
instance (Applicative f) => Applicative (Fallback f) where
    pure = Success
    Success f <*> Success x = Success (f x)
    Success f <*> Failure y = Failure (f <$> y)
    Failure f <*> Success x = Failure (($ x) <$> f)
    Failure f <*> Failure y = Failure (f <*> y)

-- | Succeed if any alternative succeeds.
instance Alternative f => Alternative (Fallback f) where
    empty = Failure empty
    Success x <|> _ = Success x
    Failure _ <|> Success y = Success y
    Failure x <|> Failure y = Failure (x <|> y)

-- | Projection to the fallback functor.
fallback :: Applicative f => Fallback f a -> f a
fallback (Success x) = pure x
fallback (Failure e) = e

-- | An applicative functor that collects a monoid (e.g. lists) of errors.
type Errors e = Fallback (Constant e)

-- | Report an error.
failure :: Monoid e => e -> Errors e a
failure e = Failure (Constant e)
