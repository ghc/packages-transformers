-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is lazy; for a strict version, see
-- "Control.Monad.Trans.RWS.Strict", which has the same interface.
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS (
    module Control.Monad.Trans.RWS.Lazy
  ) where

import Control.Monad.Trans.RWS.Lazy
