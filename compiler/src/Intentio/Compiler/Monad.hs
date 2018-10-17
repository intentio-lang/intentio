module Intentio.Compiler.Monad
  ( -- * Compile context
    CompileCtx(..)
  , compileDiagnostics
  , compileComponents
  , mkCompileCtx

    -- ** Lenses
  , component
  , requireComponent

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
  , pushIce
  , pushIceFor
  )
where

import           Intentio.Prelude        hiding ( StateT(..) )

import qualified Control.Monad.Fail            as Fail
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )
import           Data.Typeable                  ( typeRep )
import           System.IO.Error                ( tryIOError )

import           Intentio.Diagnostics           ( Diagnostic
                                                , DiagnosticSeverity(..)
                                                , diagnosticSeverity
                                                , ice
                                                , iceFor
                                                , SourcePos
                                                , HasSourcePos
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
  CompileCtx { _compileDiagnostics = empty, _compileComponents = TM.empty }

component :: forall a . Typeable a => Lens' CompileCtx (Maybe a)
component = compileComponents . TM.at @a

requireComponent :: forall a m . (Typeable a, Monad m) => CompileT m a
requireComponent = use (component @a) >>= \case
  Just c  -> return c
  Nothing -> pushIceFor () $ "no component named " <> name
  where name = show $ typeRep (Proxy @a)

--------------------------------------------------------------------------------
-- Compile monad

type Compile = CompileT IO

type CompilePure = CompileT Identity

newtype CompileT m a = CompileT { unCompileT :: MaybeT (StateT CompileCtx m) a }
  deriving (Functor, Applicative, Alternative, MonadPlus, MonadFix, MonadIO)

instance Monad m => Monad (CompileT m) where
  (CompileT m) >>= k = CompileT $ m >>= (unCompileT . k)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

  return = pure
  {-# INLINE return #-}

  fail = Fail.fail
  {-# INLINE fail #-}

instance Monad m => Fail.MonadFail (CompileT m) where
  fail = pushIceFor () . toS
  {-# INLINE fail #-}

instance Monad m => MonadState CompileCtx (CompileT m) where
  state = CompileT . lift . state
  {-# INLINE state #-}

  get = CompileT . lift $ get
  {-# INLINE get #-}

  put = CompileT . lift . put
  {-# INLINE put #-}

instance MonadTrans CompileT where
  lift = CompileT . lift . lift
  {-# INLINE lift #-}

liftIOE :: (MonadIO m) => IO a -> CompileT m a
liftIOE m = liftIO (tryIOError m) >>= perr
 where
  perr (Right x ) = return x
  perr (Left  ex) = pushIceFor () $ show ex

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

pushIce :: forall a m . (Monad m) => SourcePos -> Text -> CompileT m a
pushIce s t = pushDiagnostic (ice s t) >> unreachable

pushIceFor
  :: forall a m s . (Monad m, HasSourcePos s) => s -> Text -> CompileT m a
pushIceFor s t = pushDiagnostic (iceFor s t) >> unreachable

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
