module Intentio.Compiler.Monad
  ( -- * Compile context
    CompileCtx(..)
  , compileDiagnosticsStack
  , mkCompileCtx

    -- * Compile monad
  , Compile
  , CompilePure
  , CompileT
  , impurify

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

    -- * Methods
    -- ** Diagnostics
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

--------------------------------------------------------------------------------
-- Compile context

-- | Represents state of compilation process
data CompileCtx = CompileCtx
  { _compileDiagnosticsStack :: [Diagnostic]
  } deriving (Show, Eq)
makeLenses ''CompileCtx

-- | Construct empty compile context
mkCompileCtx :: CompileCtx
mkCompileCtx = CompileCtx {_compileDiagnosticsStack = []}

--------------------------------------------------------------------------------
-- Compile monad

type Compile = CompileT IO

type CompilePure = CompileT Identity

newtype CompileT m a = CompileT (MaybeT (StateT CompileCtx m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus,
            MonadFix, MonadIO)

instance MonadTrans CompileT where
  lift = CompileT . lift . lift
  {-# INLINE lift #-}

instance Monad m => MonadState CompileCtx (CompileT m) where
  state = CompileT . lift . state
  {-# INLINE state #-}

  get = CompileT . lift $ get
  {-# INLINE get #-}

  put = CompileT . lift . put
  {-# INLINE put #-}

-- | Convert pure compilation to I/O compilation.
impurify :: CompilePure a -> Compile a
impurify m = CompileT . MaybeT $ StateT (return . runCompilePure m)
{-# INLINE impurify #-}

--------------------------------------------------------------------------------
-- Running compilation

runCompile :: Compile a -> CompileCtx -> IO (Maybe a, CompileCtx)
runCompile = runCompileT
{-# INLINE runCompile #-}

runCompilePure :: CompilePure a -> CompileCtx -> (Maybe a, CompileCtx)
runCompilePure m c = runIdentity (runCompileT m c)
{-# INLINE runCompilePure #-}

runCompileT :: Monad m => CompileT m a -> CompileCtx -> m (Maybe a, CompileCtx)
runCompileT (CompileT m) = runStateT (runMaybeT m)
{-# INLINE runCompileT #-}

runCompileFresh :: Compile a -> IO (Maybe a, CompileCtx)
runCompileFresh m = runCompile m mkCompileCtx
{-# INLINE runCompileFresh #-}

runCompilePureFresh :: CompilePure a -> (Maybe a, CompileCtx)
runCompilePureFresh m = runCompilePure m mkCompileCtx
{-# INLINE runCompilePureFresh #-}

runCompileFreshT :: Monad m => CompileT m a -> m (Maybe a, CompileCtx)
runCompileFreshT m = runCompileT m mkCompileCtx
{-# INLINE runCompileFreshT #-}

compile :: Compile a -> CompileCtx -> IO (Either [Diagnostic] a)
compile m c = runTupleToEither <$> runCompile m c
{-# INLINE compile #-}

compilePure :: CompilePure a -> CompileCtx -> Either [Diagnostic] a
compilePure m c = runTupleToEither $ runCompilePure m c
{-# INLINE compilePure #-}

compileT :: Monad m => CompileT m a -> CompileCtx -> m (Either [Diagnostic] a)
compileT m c = runTupleToEither <$> runCompileT m c
{-# INLINE compileT #-}

compileFresh :: Compile a -> IO (Either [Diagnostic] a)
compileFresh m = runTupleToEither <$> runCompileFresh m
{-# INLINE compileFresh #-}

compilePureFresh :: CompilePure a -> Either [Diagnostic] a
compilePureFresh m = runTupleToEither $ runCompilePureFresh m
{-# INLINE compilePureFresh #-}

compileFreshT :: Monad m => CompileT m a -> m (Either [Diagnostic] a)
compileFreshT m = runTupleToEither <$> runCompileFreshT m
{-# INLINE compileFreshT #-}

runTupleToEither :: (Maybe a, CompileCtx) -> Either [Diagnostic] a
runTupleToEither (Nothing, c) = Left (c ^. compileDiagnosticsStack & reverse)
runTupleToEither (Just a , _) = Right a
{-# INLINE runTupleToEither #-}

--------------------------------------------------------------------------------
-- Methods

pushDiagnostic :: Monad m => Diagnostic -> CompileT m ()
pushDiagnostic d = do
  compileDiagnosticsStack %= (d :)
  CompileT . MaybeT . return $ if not (isDiagnosticErroneous d)
    then Just ()
    else Nothing
