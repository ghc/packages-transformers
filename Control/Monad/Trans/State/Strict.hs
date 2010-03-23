-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict state monads.
--
-- See below for examples.
-----------------------------------------------------------------------------

module Control.Monad.Trans.State.Strict (
    -- * The State monad
    State,
    state,
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
    -- * State operations
    get,
    put,
    modify,
    gets,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
    liftListen,
    liftPass,
    -- * Examples
    -- $examples
  ) where

import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

-- ---------------------------------------------------------------------------
-- | A parameterizable state monad where /s/ is the type of the state
-- to carry and /a/ is the type of the /return value/.

type State s = StateT s Identity

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: (s -> (a, s)) -> State s a
state f = StateT (Identity . f)

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

-- |Evaluate this state monad with the given initial state,throwing
-- away the final state.  Very much like @fst@ composed with
-- @runstate@.

evalState :: State s a -- ^The state to evaluate
          -> s         -- ^An initial value
          -> a         -- ^The return value of the state application
evalState m s = fst (runState m s)

-- |Execute this state and return the new state, throwing away the
-- return value.  Very much like @snd@ composed with
-- @runstate@.

execState :: State s a -- ^The state to evaluate
          -> s         -- ^An initial value
          -> s         -- ^The new state
execState m s = snd (runState m s)

-- |Map a stateful computation from one (return value, state) pair to
-- another.  For instance, to convert numberTree from a function that
-- returns a tree to a function that returns the sum of the numbered
-- tree (see the Examples section for numberTree and sumTree) you may
-- write:
--
-- > sumNumberedTree :: (Eq a) => Tree a -> State (Table a) Int
-- > sumNumberedTree = mapState (\ (t, tab) -> (sumTree t, tab))  . numberTree

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

-- |Apply this function to this state and return the resulting state.
withState :: (s -> s) -> State s a -> State s a
withState = withStateT

-- ---------------------------------------------------------------------------
-- | A parameterizable state monad for encapsulating an inner
-- monad.
--
-- The StateT Monad structure is parameterized over two things:
--
--   * s - The state.
--
--   * m - The inner monad.
--
-- Here are some examples of use:
--
-- (Parser from ParseLib with Hugs)
--
-- >  type Parser a = StateT String [] a
-- >     ==> StateT (String -> [(a,String)])
--
-- For example, item can be written as:
--
-- >   item = do (x:xs) <- get
-- >          put xs
-- >          return x
-- >
-- >   type BoringState s a = StateT s Identity a
-- >        ==> StateT (s -> Identity (a,s))
-- >
-- >   type StateWithIO s a = StateT s IO a
-- >        ==> StateT (s -> IO (a,s))
-- >
-- >   type StateWithErr s a = StateT s Maybe a
-- >        ==> StateT (s -> Maybe (a,s))

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

-- | Map a stateful computation from one (return value, state) pair to
-- another.  For instance, to convert numberTree from a function that
-- returns a tree to a function that returns the sum of the numbered
-- tree (see the Examples section for numberTree and sumTree) you may
-- write:
--
-- > sumNumberedTree :: (Eq a) => Tree a -> State (Table a) Int
-- > sumNumberedTree = mapState (\ (t, tab) -> (sumTree t, tab))  . numberTree

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | Apply this function to this state and return the resulting state.
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero       = StateT $ \_ -> mzero
    m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance (MonadFix m) => MonadFix (StateT s m) where
    mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = do
    s <- get
    put (f s)

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = do
    s <- get
    return (f s)

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: ((((a,s) -> m (b,s)) -> m (a,s)) -> m (a,s)) ->
    ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
liftCallCC callCC f = StateT $ \s ->
    callCC $ \c ->
    runStateT (f (\a -> StateT $ \ _ -> c (a, s))) s

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the laws of a monad transformer.
liftCallCC' :: ((((a,s) -> m (b,s)) -> m (a,s)) -> m (a,s)) ->
    ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
liftCallCC' callCC f = StateT $ \s ->
    callCC $ \c ->
    runStateT (f (\a -> StateT $ \s' -> c (a, s'))) s

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a,s) -> (e -> m (a,s)) -> m (a,s)) ->
    StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatch catchError m h =
    StateT $ \s -> runStateT m s `catchError` \e -> runStateT (h e) s

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m =>
    (m (a,s) -> m ((a,s),w)) -> StateT s m a -> StateT s m (a,w)
liftListen listen m = StateT $ \s -> do
    ((a, s'), w) <- listen (runStateT m s)
    return ((a, w), s')

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m =>
    (m ((a,s),b) -> m (a,s)) -> StateT s m (a,b) -> StateT s m a
liftPass pass m = StateT $ \s -> pass $ do
    ((a, f), s') <- runStateT m s
    return ((a, s'), f)

{- $examples

A function to increment a counter.  Taken from the paper
/Generalising Monads to Arrows/, John
Hughes (<http://www.math.chalmers.se/~rjmh/>), November 1998:

> tick :: State Int Int
> tick = do n <- get
>           put (n+1)
>           return n

Add one to the given number using the state monad:

> plusOne :: Int -> Int
> plusOne n = execState tick n

A contrived addition example. Works only with positive numbers:

> plus :: Int -> Int -> Int
> plus n x = execState (sequence $ replicate n tick) x

An example from /The Craft of Functional Programming/, Simon
Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
tree of integers in which the original elements are replaced by
natural numbers, starting from 0.  The same element has to be
replaced by the same number at every occurrence, and when we meet
an as-yet-unvisited element we have to find a \'new\' number to match
it with:\"

> data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
> type Table a = [a]

> numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
> numberTree Nil = return Nil
> numberTree (Node x t1 t2)
>        =  do num <- numberNode x
>              nt1 <- numberTree t1
>              nt2 <- numberTree t2
>              return (Node num nt1 nt2)
>     where
>     numberNode :: Eq a => a -> State (Table a) Int
>     numberNode x
>        = do table <- get
>             (newTable, newPos) <- return (nNode x table)
>             put newTable
>             return newPos
>     nNode::  (Eq a) => a -> Table a -> (Table a, Int)
>     nNode x table
>        = case (findIndexInList (== x) table) of
>          Nothing -> (table ++ [x], length table)
>          Just i  -> (table, i)
>     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
>     findIndexInList = findIndexInListHelp 0
>     findIndexInListHelp _ _ [] = Nothing
>     findIndexInListHelp count f (h:t)
>        = if (f h)
>          then Just count
>          else findIndexInListHelp (count+1) f t

numTree applies numberTree with an initial state:

> numTree :: (Eq a) => Tree a -> Tree Int
> numTree t = evalState (numberTree t) []

> testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
> numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil

sumTree is a little helper function that does not use the State monad:

> sumTree :: (Num a) => Tree a -> a
> sumTree Nil = 0
> sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)
-}
