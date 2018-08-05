module Intentio.Compiler.Monad
  ( -- * Compile monad
    Compile
  , CompilePure
  , CompileT
  , impurify

    -- ** Running compilation
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
  )
where

import           Intentio.Prelude        hiding ( moduleName
                                                , StateT(..)
                                                )

import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.Fix              ( MonadFix(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )

import           Intentio.Compiler.Context      -- Import all
import           Intentio.Diagnostics           ( Diagnostic )

--------------------------------------------------------------------------------
-- Compile monad

type Compile = CompileT IO

type CompilePure = CompileT Identity

newtype CompileT m a = CompileT (MaybeT (StateT CompileCtx m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus,
            MonadFix, MonadIO)

instance MonadTrans CompileT where
  lift = lift
  {-# INLINE lift #-}

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
runCompileFresh m = runCompile m freshCompileCtx
{-# INLINE runCompileFresh #-}

runCompilePureFresh :: CompilePure a -> (Maybe a, CompileCtx)
runCompilePureFresh m = runCompilePure m freshCompileCtx
{-# INLINE runCompilePureFresh #-}

runCompileFreshT :: Monad m => CompileT m a -> m (Maybe a, CompileCtx)
runCompileFreshT m = runCompileT m freshCompileCtx
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
runTupleToEither (Nothing, c) = Left (c ^. compileDiagnostics)
runTupleToEither (Just a , _) = Right a
{-# INLINE runTupleToEither #-}
