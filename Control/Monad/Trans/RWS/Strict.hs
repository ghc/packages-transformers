-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict RWS monad.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS.Strict (
    -- * The RWS monad
    RWS,
    rws,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,
    -- * The RWST monad transformer
    RWST(..),
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    -- * Reader operations
    ask,
    local,
    asks,
    -- * Writer operations
    tell,
    listen,
    pass,
    listens,
    censor,
    -- * State operations
    get,
    put,
    modify,
    gets,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Monoid

type RWS r w s = RWST r w s Identity

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws f = RWST (\ r s -> Identity (f r s))

runRWS :: RWS r w s a -> r -> s -> (a, s, w)
runRWS m r s = runIdentity (runRWST m r s)

evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS m r s = let
    (a, _, w) = runRWS m r s
    in (a, w)

execRWS :: RWS r w s a -> r -> s -> (s, w)
execRWS m r s = let
    (_, s', w) = runRWS m r s
    in (s', w)

mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f = mapRWST (Identity . f . runIdentity)

withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS = withRWST

-- ---------------------------------------------------------------------------
-- Our parameterizable RWS monad, with an inner monad

newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

evalRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (a, w)
evalRWST m r s = do
    (a, _, w) <- runRWST m r s
    return (a, w)

execRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST m r s = do
    (_, s', w) <- runRWST m r s
    return (s', w)

mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \r s -> f (runRWST m r s)

withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \r s -> uncurry (runRWST m) (f r s)

instance (Monad m) => Functor (RWST r w s m) where
    fmap f m = RWST $ \r s -> do
        (a, s', w) <- runRWST m r s
        return (f a, s', w)

instance (Monoid w, Monad m) => Applicative (RWST r w s m) where
    pure = return
    (<*>) = ap

instance (Monoid w, Monad m) => Monad (RWST r w s m) where
    return a = RWST $ \_ s -> return (a, s, mempty)
    m >>= k  = RWST $ \r s -> do
        (a, s', w)  <- runRWST m r s
        (b, s'',w') <- runRWST (k a) r s'
        return (b, s'', w `mappend` w')
    fail msg = RWST $ \_ _ -> fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (RWST r w s m) where
    mzero       = RWST $ \_ _ -> mzero
    m `mplus` n = RWST $ \r s -> runRWST m r s `mplus` runRWST n r s

instance (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
    mfix f = RWST $ \r s -> mfix $ \ ~(a, _, _) -> runRWST (f a) r s

instance (Monoid w) => MonadTrans (RWST r w s) where
    lift m = RWST $ \_ s -> do
        a <- m
        return (a, s, mempty)

instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO

-- ---------------------------------------------------------------------------
-- Reader operations

ask :: (Monoid w, Monad m) => RWST r w s m r
ask = RWST $ \r s -> return (r, s, mempty)

local :: (Monoid w, Monad m) => (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST $ \r s -> runRWST m (f r) s

asks :: (Monoid w, Monad m) => (r -> a) -> RWST r w s m a
asks f = do
    r <- ask
    return (f r)

-- ---------------------------------------------------------------------------
-- Writer operations

tell :: (Monoid w, Monad m) => w -> RWST r w s m ()
tell w = RWST $ \_ s -> return ((),s,w)

listen :: (Monoid w, Monad m) => RWST r w s m a -> RWST r w s m (a, w)
listen m = RWST $ \r s -> do
    (a, s', w) <- runRWST m r s
    return ((a, w), s', w)

pass :: (Monoid w, Monad m) => RWST r w s m (a, w -> w) -> RWST r w s m a
pass m = RWST $ \r s -> do
    ((a, f), s', w) <- runRWST m r s
    return (a, s', f w)

listens :: (Monoid w, Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (a, b)
listens f m = do
    (a, w) <- listen m
    return (a, f w)
 
censor :: (Monoid w, Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- ---------------------------------------------------------------------------
-- State operations

get :: (Monoid w, Monad m) => RWST r w s m s
get = RWST $ \_ s -> return (s, s, mempty)

put :: (Monoid w, Monad m) => s -> RWST r w s m ()
put s = RWST $ \_ _ -> return ((), s, mempty)

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
modify :: (Monoid w, Monad m) => (s -> s) -> RWST r w s m ()
modify f = do
    s <- get
    put (f s)
 
-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (Monoid w, Monad m) => (s -> a) -> RWST r w s m a
gets f = do
    s <- get
    return (f s)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) =>
    ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ->
    ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a
liftCallCC callCC f = RWST $ \r s ->
    callCC $ \c ->
    runRWST (f (\a -> RWST $ \_ s' -> c (a, s', mempty))) r s

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a,s,w) -> (e -> m (a,s,w)) -> m (a,s,w)) ->
    RWST l w s m a -> (e -> RWST l w s m a) -> RWST l w s m a
liftCatch catchError m h =
    RWST $ \r s -> runRWST m r s `catchError` \e -> runRWST (h e) r s
