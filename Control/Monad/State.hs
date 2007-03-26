{-# OPTIONS -fallow-undecidable-instances #-}
-- Search for -fallow-undecidable-instances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- State monads.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State (
    -- * MonadState class
    MonadState(..),
    modify,
    gets,
    -- * The State monad
    State,
    runState,
    evalState,
    execState,
    mapState,
    withState,
    -- * The StateT monad transformer
    StateT(..),
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Examples
    -- $examples
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, get, put)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, get, put)
import Control.Monad.Trans.State
        (State, runState, evalState, execState, mapState, withState,
         StateT(..), evalStateT, execStateT, mapStateT, withStateT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, get, put)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans
import Data.Monoid

-- ---------------------------------------------------------------------------
-- | /get/ returns the state from the internals of the monad.
--
-- /put/ replaces the state inside the monad.

class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
    s <- get
    put (f s)

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance (Monad m) => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put

instance (Monad m) => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put

instance (Monad m, Monoid w) => MonadState s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put

instance (Monad m, Monoid w) => MonadState s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ContT r m) where
    get = lift get
    put = lift . put

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (ListT m) where
    get = lift get
    put = lift . put

-- Needs -fallow-undecidable-instances
instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

-- Needs -fallow-undecidable-instances
instance (Monoid w, MonadState s m) => MonadState s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put

-- Needs -fallow-undecidable-instances
instance (Monoid w, MonadState s m) => MonadState s (Strict.WriterT w m) where
    get = lift get
    put = lift . put

-- ---------------------------------------------------------------------------
-- $examples
-- A function to increment a counter.  Taken from the paper
-- /Generalising Monads to Arrows/, John
-- Hughes (<http://www.math.chalmers.se/~rjmh/>), November 1998:
--
-- > tick :: State Int Int
-- > tick = do n <- get
-- >           put (n+1)
-- >           return n
--
-- Add one to the given number using the state monad:
--
-- > plusOne :: Int -> Int
-- > plusOne n = execState tick n
--
-- A contrived addition example. Works only with positive numbers:
--
-- > plus :: Int -> Int -> Int
-- > plus n x = execState (sequence $ replicate n tick) x
--
-- An example from /The Craft of Functional Programming/, Simon
-- Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
-- Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
-- tree of integers in which the original elements are replaced by
-- natural numbers, starting from 0.  The same element has to be
-- replaced by the same number at every occurrence, and when we meet
-- an as-yet-unvisited element we have to find a \'new\' number to match
-- it with:\"
--
-- > data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- > type Table a = [a]
--
-- > numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
-- > numberTree Nil = return Nil
-- > numberTree (Node x t1 t2)
-- >        =  do num <- numberNode x
-- >              nt1 <- numberTree t1
-- >              nt2 <- numberTree t2
-- >              return (Node num nt1 nt2)
-- >     where
-- >     numberNode :: Eq a => a -> State (Table a) Int
-- >     numberNode x
-- >        = do table <- get
-- >             (newTable, newPos) <- return (nNode x table)
-- >             put newTable
-- >             return newPos
-- >     nNode::  (Eq a) => a -> Table a -> (Table a, Int)
-- >     nNode x table
-- >        = case (findIndexInList (== x) table) of
-- >          Nothing -> (table ++ [x], length table)
-- >          Just i  -> (table, i)
-- >     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
-- >     findIndexInList = findIndexInListHelp 0
-- >     findIndexInListHelp _ _ [] = Nothing
-- >     findIndexInListHelp count f (h:t)
-- >        = if (f h)
-- >          then Just count
-- >          else findIndexInListHelp (count+1) f t
--
-- numTree applies numberTree with an initial state:
--
-- > numTree :: (Eq a) => Tree a -> Tree Int
-- > numTree t = evalState (numberTree t) []
--
-- > testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
-- > numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
--
-- sumTree is a little helper function that does not use the State monad:
--
-- > sumTree :: (Num a) => Tree a -> a
-- > sumTree Nil = 0
-- > sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)
