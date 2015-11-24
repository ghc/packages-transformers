{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.List
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The ListT monad transformer, adding backtracking to a given monad,
-- which must be commutative.
-----------------------------------------------------------------------------

module Control.Monad.Trans.List (
    -- * The ListT monad transformer
    ListT(..),
    mapListT,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Classes

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

-- | Parameterizable list monad, with an inner monad.
--
-- /Note:/ this does not yield a monad unless the argument monad is commutative.
newtype ListT m a = ListT { runListT :: m [a] }

instance (Eq1 m) => Eq1 (ListT m) where
    eqWith eq (ListT x) (ListT y) = eqWith (eqWith eq) x y

instance (Ord1 m) => Ord1 (ListT m) where
    compareWith comp (ListT x) (ListT y) = compareWith (compareWith comp) x y

instance (Read1 m) => Read1 (ListT m) where
    readsPrecWith rp = readsData $
        readsUnaryWith (readsPrecWith (readsPrecWith rp)) "ListT" ListT

instance (Show1 m) => Show1 (ListT m) where
    showsPrecWith sp d (ListT m) =
        showsUnaryWith (showsPrecWith (showsPrecWith sp)) "ListT" d m

instance (Eq1 m, Eq a) => Eq (ListT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (ListT m a) where compare = compare1
instance (Read1 m, Read a) => Read (ListT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (ListT m a) where showsPrec = showsPrec1

-- | Map between 'ListT' computations.
--
-- * @'runListT' ('mapListT' f m) = f ('runListT' m)@
mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f m = ListT $ f (runListT m)

instance (Functor m) => Functor (ListT m) where
    fmap f = mapListT $ fmap $ map f

instance (Foldable f) => Foldable (ListT f) where
    foldMap f (ListT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (ListT f) where
    traverse f (ListT a) = ListT <$> traverse (traverse f) a

instance (Applicative m) => Applicative (ListT m) where
    pure a  = ListT $ pure [a]
    f <*> v = ListT $ (<*>) <$> runListT f <*> runListT v

instance (Applicative m) => Alternative (ListT m) where
    empty   = ListT $ pure []
    m <|> n = ListT $ (++) <$> runListT m <*> runListT n

instance (Monad m) => Monad (ListT m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = ListT $ return [a]
#endif
    m >>= k  = ListT $ do
        a <- runListT m
        b <- mapM (runListT . k) a
        return (concat b)
    fail _ = ListT $ return []

#if MIN_VERSION_base(4,9,0)
instance (Monad m) => Fail.MonadFail (ListT m) where
    fail _ = ListT $ return []
#endif

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

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (ListT m) where
    mzipWith f (ListT a) (ListT b) = ListT $ mzipWith (zipWith f) a b
#endif

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m [a] [b] -> CallCC (ListT m) a b
liftCallCC callCC f = ListT $
    callCC $ \ c ->
    runListT (f (\ a -> ListT $ c [a]))

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m [a] -> Catch e (ListT m) a
liftCatch catchE m h = ListT $ runListT m
    `catchE` \ e -> runListT (h e)
