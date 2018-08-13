module Intentio.Compiler.Monad
  ( -- * Compile context
    CompileCtx(..)
  , compileDiagnosticsStack
  , compileComponents
  , mkCompileCtx

    -- ** Lenses
  , component

    -- * Compile monad
  , Compile
  , CompilePure
  , CompileT

    -- * Running compilation
  , runCompile
  , runCompilePure
  , runCompileT
  , runCompileFresh
  , runCompilePureFresh
  , runCompileFreshT
  , compile
  , compilePure
  , compileT
  , compileFresh
  , compilePureFresh
  , compileFreshT

    -- * Operations on compilation monad
  , impurify
  , fork

    -- * Methods
  , pushDiagnostic
  )
where

import           Intentio.Prelude        hiding ( moduleName
                                                , StateT(..)
                                                )

import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )

import           Intentio.Diagnostics           ( Diagnostic
                                                , isDiagnosticErroneous
                                                )
import qualified Intentio.TypeMap              as TM

--------------------------------------------------------------------------------
-- Compile context

-- | Represents state of compilation process
data CompileCtx = CompileCtx
  { _compileDiagnosticsStack :: [Diagnostic],
    _compileComponents :: TM.TypeMap
  } deriving (Show)
makeLenses ''CompileCtx

-- | Construct empty compile context
mkCompileCtx :: CompileCtx
mkCompileCtx =
  CompileCtx {_compileDiagnosticsStack = [], _compileComponents = TM.empty}

component :: forall a . Typeable a => Lens' CompileCtx (Maybe a)
component = compileComponents . TM.at @a

--------------------------------------------------------------------------------
-- Compile monad

type Compile = CompileT IO

type CompilePure = CompileT Identity

newtype CompileT m a = CompileT (MaybeT (StateT CompileCtx m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus,
            MonadFix, MonadIO)

instance MonadTrans CompileT where
  lift = CompileT . lift . lift
  {-# INLINABLE lift #-}

instance Monad m => MonadState CompileCtx (CompileT m) where
  state = CompileT . lift . state
  {-# INLINABLE state #-}

  get = CompileT . lift $ get
  {-# INLINABLE get #-}

  put = CompileT . lift . put
  {-# INLINABLE put #-}

--------------------------------------------------------------------------------
-- Running compilation

runCompile :: Compile a -> CompileCtx -> IO (Maybe a, CompileCtx)
runCompile = runCompileT
{-# INLINABLE runCompile #-}

runCompilePure :: CompilePure a -> CompileCtx -> (Maybe a, CompileCtx)
runCompilePure m c = runIdentity (runCompileT m c)
{-# INLINABLE runCompilePure #-}

runCompileT :: Monad m => CompileT m a -> CompileCtx -> m (Maybe a, CompileCtx)
runCompileT (CompileT m) = runStateT (runMaybeT m)
{-# INLINABLE runCompileT #-}

runCompileFresh :: Compile a -> IO (Maybe a, CompileCtx)
runCompileFresh m = runCompile m mkCompileCtx
{-# INLINABLE runCompileFresh #-}

runCompilePureFresh :: CompilePure a -> (Maybe a, CompileCtx)
runCompilePureFresh m = runCompilePure m mkCompileCtx
{-# INLINABLE runCompilePureFresh #-}

runCompileFreshT :: Monad m => CompileT m a -> m (Maybe a, CompileCtx)
runCompileFreshT m = runCompileT m mkCompileCtx
{-# INLINABLE runCompileFreshT #-}

compile :: Compile a -> CompileCtx -> IO (Either [Diagnostic] a)
compile m c = runTupleToEither <$> runCompile m c
{-# INLINABLE compile #-}

compilePure :: CompilePure a -> CompileCtx -> Either [Diagnostic] a
compilePure m c = runTupleToEither $ runCompilePure m c
{-# INLINABLE compilePure #-}

compileT :: Monad m => CompileT m a -> CompileCtx -> m (Either [Diagnostic] a)
compileT m c = runTupleToEither <$> runCompileT m c
{-# INLINABLE compileT #-}

compileFresh :: Compile a -> IO (Either [Diagnostic] a)
compileFresh m = runTupleToEither <$> runCompileFresh m
{-# INLINABLE compileFresh #-}

compilePureFresh :: CompilePure a -> Either [Diagnostic] a
compilePureFresh m = runTupleToEither $ runCompilePureFresh m
{-# INLINABLE compilePureFresh #-}

compileFreshT :: Monad m => CompileT m a -> m (Either [Diagnostic] a)
compileFreshT m = runTupleToEither <$> runCompileFreshT m
{-# INLINABLE compileFreshT #-}

runTupleToEither :: (Maybe a, CompileCtx) -> Either [Diagnostic] a
runTupleToEither (Nothing, c) = Left (c ^. compileDiagnosticsStack & reverse)
runTupleToEither (Just a , _) = Right a
{-# INLINABLE runTupleToEither #-}

--------------------------------------------------------------------------------
-- Operations on compilation monad

-- | Convert pure compilation to I/O compilation.
impurify :: CompilePure a -> Compile a
impurify m = CompileT . MaybeT $ StateT (return . runCompilePure m)
{-# INLINABLE impurify #-}

-- | Run multiple independent compilations.
fork :: (Monad m, Traversable t) => t (CompileT m a) -> CompileT m (t a)
fork = sequence -- TODO: Make this run in parallel

--------------------------------------------------------------------------------
-- Methods

pushDiagnostic :: Monad m => Diagnostic -> CompileT m ()
pushDiagnostic d = do
  compileDiagnosticsStack %= (d :)
  CompileT . MaybeT . return $ if not (isDiagnosticErroneous d)
    then Just ()
    else Nothing
