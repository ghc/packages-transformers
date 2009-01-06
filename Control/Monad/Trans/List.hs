-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.List
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The List monad.
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.List (
    -- * The ListT monad transformer
    ListT(..),
    mapListT,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- | Parameterizable list monad, with an inner monad.
--
-- /Note:/ this does not yield a monad unless the argument monad is commutative.
newtype ListT m a = ListT { runListT :: m [a] }

mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f m = ListT $ f (runListT m)

instance (Functor m) => Functor (ListT m) where
    fmap f = mapListT $ fmap $ map f

instance (Applicative m) => Applicative (ListT m) where
    pure a = ListT $ pure [a]
    ListT f <*> ListT v = ListT $ liftA2 (<*>) f v

instance (Monad m) => Monad (ListT m) where
    return a = ListT $ return [a]
    m >>= k  = ListT $ do
        a <- runListT m
        b <- mapM (runListT . k) a
        return (concat b)
    fail _ = ListT $ return []

instance (Monad m) => MonadPlus (ListT m) where
    mzero       = ListT $ return []
    m `mplus` n = ListT $ do
        a <- runListT m
        b <- runListT n
        return (a ++ b)

instance MonadTrans ListT where
    lift m = ListT $ do
        a <- m
        return [a]

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO = lift . liftIO

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: ((([a] -> m [b]) -> m [a]) -> m [a]) ->
    ((a -> ListT m b) -> ListT m a) -> ListT m a
liftCallCC callCC f = ListT $
    callCC $ \c ->
    runListT (f (\a -> ListT $ c [a]))

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m [a] -> (e -> m [a]) -> m [a]) ->
    ListT m a -> (e -> ListT m a) -> ListT m a
liftCatch catchError m h = ListT $ runListT m
    `catchError` \e -> runListT (h e)
