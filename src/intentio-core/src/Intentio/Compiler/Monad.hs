module Intentio.Compiler.Monad
  ( -- * Compile context
    CompileCtx(..)
  , compileDiagnostics
  , compileComponents
  , mkCompileCtx

    -- ** Lenses
  , component

    -- * Compile monad
  , Compile
  , CompilePure
  , CompileT
  , liftIOE

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

    -- * Methods
  , currentDiagnosticSeverity
  , pushDiagnostic
  , pushDiagnostics
  , pushDiagnosticE
  , pushDiagnosticsE
  )
where

import           Intentio.Prelude        hiding ( StateT(..) )

import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )
import           System.IO.Error                ( tryIOError )

import           Intentio.Diagnostics           ( Diagnostic
                                                , DiagnosticSeverity(..)
                                                , diagnosticSeverity
                                                , iceFor
                                                )
import qualified Intentio.TypeMap              as TM

--------------------------------------------------------------------------------
-- Compile context

-- | Represents state of compilation process
data CompileCtx = CompileCtx
  { _compileDiagnostics :: Seq Diagnostic,
    _compileComponents :: TM.TypeMap
  } deriving (Show)
makeLenses ''CompileCtx

-- | Construct empty compile context
mkCompileCtx :: CompileCtx
mkCompileCtx =
  CompileCtx {_compileDiagnostics = empty, _compileComponents = TM.empty}

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

liftIOE :: (MonadIO m) => IO a -> CompileT m a
liftIOE m = liftIO (tryIOError m) >>= perr
 where
  perr (Right x ) = return x
  perr (Left  ex) = pushDiagnostic (mkDiag ex) >> unreachable

  mkDiag = iceFor () . show

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
runTupleToEither (Nothing, c) = Left (c ^. compileDiagnostics & toList)
runTupleToEither (Just a , _) = Right a
{-# INLINABLE runTupleToEither #-}

--------------------------------------------------------------------------------
-- Operations on compilation monad

-- | Convert pure compilation to I/O compilation.
impurify :: CompilePure a -> Compile a
impurify m = CompileT . MaybeT $ StateT (return . runCompilePure m)
{-# INLINABLE impurify #-}

--------------------------------------------------------------------------------
-- Methods

currentDiagnosticSeverity :: Monad m => CompileT m DiagnosticSeverity
currentDiagnosticSeverity =
  use compileDiagnostics
    <&> fmap (^. diagnosticSeverity)
    <&> foldr' max minBound

pushDiagnostic :: Monad m => Diagnostic -> CompileT m ()
pushDiagnostic = pushDiagnostic_ DiagnosticError

pushDiagnostics :: (Foldable t, Monad m) => t Diagnostic -> CompileT m ()
pushDiagnostics = pushDiagnostics_ DiagnosticError

pushDiagnosticE :: Monad m => Diagnostic -> CompileT m ()
pushDiagnosticE = pushDiagnostic_ DiagnosticICE

pushDiagnosticsE :: (Foldable t, Monad m) => t Diagnostic -> CompileT m ()
pushDiagnosticsE = pushDiagnostics_ DiagnosticICE

pushDiagnostic_ :: Monad m => DiagnosticSeverity -> Diagnostic -> CompileT m ()
pushDiagnostic_ s d = do
  compileDiagnostics %= (|> d)
  CompileT . MaybeT . return $ if isErr then Nothing else Just ()
  where isErr = d ^. diagnosticSeverity >= s

pushDiagnostics_
  :: (Foldable t, Monad m)
  => DiagnosticSeverity
  -> t Diagnostic
  -> CompileT m ()
pushDiagnostics_ s d = do
  compileDiagnostics %= \x -> foldl' (|>) x d
  CompileT . MaybeT . return $ if any isErr d then Nothing else Just ()
  where isErr x = x ^. diagnosticSeverity >= s
