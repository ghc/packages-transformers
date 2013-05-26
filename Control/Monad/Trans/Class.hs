-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The class of monad transformers.
--
-- A monad transformer makes a new monad out of an existing monad, such
-- that computations of the old monad may be embedded in the new one.
-- To construct a monad with a desired set of features, one typically
-- starts with a base monad, such as @Identity@, @[]@ or 'IO', and
-- applies a sequence of monad transformers.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Class (
    -- * Transformer class
    MonadTrans(..)

    -- * Conventions
    -- $conventions

    -- * Examples
    -- ** Parsing
    -- $example1

    -- ** Parsing and counting
    -- $example2

    -- ** Interpreter monad
    -- $example3
  ) where

-- | The class of monad transformers.  Instances should satisfy the
-- following laws, which state that 'lift' is a monad transformation:
--
-- * @'lift' . 'return' = 'return'@
--
-- * @'lift' (m >>= f) = 'lift' m >>= ('lift' . f)@

class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad.
    lift :: Monad m => m a -> t m a

{- $conventions
Most monad transformer modules include the special case of applying the
transformer to @Identity@.  For example, @State s@ is an abbreviation
for @StateT s Identity@.

Each monad transformer also comes with an operation @run@/XXX/@T@ to
unwrap the transformer, exposing a computation of the inner monad.

All of the monad transformers except @ContT@ are functors on the
category of monads: in addition to defining a mapping of monads,
they also define a mapping from transformations between base monads
to transformations between transformed monads, called @map@/XXX/@T@.
Thus given a monad transformation @t :: M a -> N a@, the combinator
@mapStateT@ constructs a monad transformation

> mapStateT t :: StateT s M a -> StateT s N a

Each of the monad transformers introduces relavant operations.
In a sequence of monad transformers, most of these operations.can be
lifted through other transformers using 'lift' or the @map@/XXX/@T@
combinator, but a few with more complex type signatures require
specialized lifting combinators, called @lift@/Op/.
-}

{- $example1

One might define a parsing monad by adding a state (the 'String' remaining
to be parsed) to the @[]@ monad, which provides non-determinism:

> import Control.Monad.Trans.State
>
> type Parser = StateT String []

Then @Parser@ is an instance of @MonadPlus@: monadic sequencing implements
concatenation of parsers, while @mplus@ provides choice.
To use parsers, we need a primitive to run a constructed parser on an
input string:

> runParser :: Parser a -> String -> [a]
> runParser p s = [x | (x, "") <- runStateT p s]

Finally, we need a primitive parser that matches a single character,
from which arbitrarily complex parsers may be constructed:

> item :: Parser Char
> item = do
>     c:cs <- get
>     put cs
>     return c

In this example we use the operations @get@ and @put@ from
"Control.Monad.Trans.State", which are defined only for monads that are
applications of @StateT@.  Alternatively one could use monad classes
from the @mtl@ package or similar, which contain methods @get@ and @put@
with types generalized over all suitable monads.
-}

{- $example2

We can define a parser that also counts by adding a @WriterT@ transformer:

> import Control.Monad.Trans.Class
> import Control.Monad.Trans.State
> import Control.Monad.Trans.Writer
> import Data.Monoid
>
> type Parser = WriterT (Sum Int) (StateT String [])

The function that applies a parser must now unwrap each of the monad
transformers in turn:

> runParser :: Parser a -> String -> [(a, Int)]
> runParser p s = [(x, n) | ((x, Sum n), "") <- runStateT (runWriterT p) s]

To define the @item@ parser, we need to lift the @StateT@ operations through
the @WriterT@ transformers.

> item :: Parser Char
> item = do
>     c:cs <- lift get
>     lift (put cs)
>     return c

In this case, we were able to do this with 'lift', but operations with
more complex types require special lifting functions, which are provided
by monad transformers for which they can be implemented.  If you use the
monad classes of the @mtl@ package or similar, this lifting is handled
automatically by the instances of the classes, and you need only use
the generalized methods @get@ and @put@.

We can also define a primitive using the Writer:

> tick :: Parser ()
> tick = tell (Sum 1)

Then the parser will keep track of how many @tick@s it executes.
-}

{- $example3

This example is a cut-down version of the one in
\"Monad Transformers and Modular Interpreters\",
by Sheng Liang, Paul Hudak and Mark Jones in /POPL'95/
(<http://web.cecs.pdx.edu/~mpj/pubs/modinterp.html>).

Suppose we want to define an interpreter that can do I\/O and has
exceptions, an environment and a modifiable store.  We can define
a monad that supports all these things as a stack of monad transformers:

> import Control.Monad.Trans.Class
> import Control.Monad.Trans.State
> import qualified Control.Monad.Trans.Reader as R
> import qualified Control.Monad.Trans.Error as E
>
> type InterpM = StateT Store (R.ReaderT Env (E.ErrorT Err []))

for suitable types @Store@, @Env@ and @Err@.

Now we would like to be able to use the operations associated with each
of those monad transformers on @InterpM@ actions.
Since the uppermost monad transformer of @InterpM@ is @StateT@,
it already has the state operations @get@ and @set@.

The first of the @ReaderT@ operations, @ask@, is a simple action,
so we can lift it through @StateT@ to @InterpM@ using 'lift':

> ask :: InterpM Env
> ask = lift R.ask

The other @ReaderT@ operation, @local@, has a suitable type for lifting
using @mapStateT@:

> local :: (Env -> Env) -> InterpM a -> InterpM a
> local f = mapStateT (R.local f)

We also wish to lift the operations of @ErrorT@ through both @ReaderT@
and @StateT@.  For the operation @throwError@, we know @throwError e@ is
and simple action, so we can lift it through the two monad transformers
to @InterpM@ with two @lift@s:

> throwError :: Err -> InterpM a
> throwError e = lift (lift (E.throwError e))

The @catchError@ operation has a more complex type, so we need to use
the special-purpose lifting function @liftCatch@ provided by most monad
transformers.  Here we use the @ReaderT@ version followed by the @StateT@
version:

> catchError :: InterpM a -> (Err -> InterpM a) -> InterpM a
> catchError = liftCatch (R.liftCatch E.catchError)

We could lift 'IO' actions to @InterpM@ using three @lift@s, but @InterpM@
is automatically an instance of @MonadIO@, so we can use @liftIO@ instead:

> putStr :: String -> InterpM ()
> putStr s = liftIO (Prelude.putStr s)

-}
