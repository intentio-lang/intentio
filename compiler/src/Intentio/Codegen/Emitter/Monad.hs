module Intentio.Codegen.Emitter.Monad
  ( ModuleEmit
  , ItemEmit
  , BodyEmit
  , ImpBodyEmit
  , runModuleEmit
  , runItemEmit
  , runBodyEmit
  , runImpBodyEmit
  , withModule
  , withItem
  , withBody
  , withImpBody
  , MonadModuleEmit(..)
  , MonadItemEmit(..)
  , MonadBodyEmit(..)
  , MonadImpBodyEmit(..)
  )
where

import           Intentio.Prelude

import qualified Intentio.Codegen.Imp          as I
import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                )
import qualified Intentio.Hir                  as H

type Emit r = ReaderT r CompilePure

type ModuleEmit = Emit ModuleContext
type ItemEmit = Emit ItemContext
type BodyEmit = Emit BodyContext
type ImpBodyEmit = Emit ImpBodyContext

type ModuleContext = (Assembly (H.Module ()), H.Module ())
type ItemContext = (Assembly (H.Module ()), H.Module (), H.Item ())
type BodyContext = (Assembly (H.Module ()), H.Module (), H.Item (), H.Body ())
type ImpBodyContext
  = (Assembly (H.Module ()), H.Module (), H.Item (), I.Body ())

runModuleEmit
  :: ModuleEmit a -> Assembly (H.Module ()) -> H.Module () -> CompilePure a
runModuleEmit f a m = runReaderT f (a, m)
{-# INLINE runModuleEmit #-}

runItemEmit :: ItemEmit a -> H.Item () -> ModuleEmit a
runItemEmit = flip withItem
{-# INLINE runItemEmit #-}

runBodyEmit :: BodyEmit a -> H.Body () -> ItemEmit a
runBodyEmit = flip withBody
{-# INLINE runBodyEmit #-}

runImpBodyEmit :: ImpBodyEmit a -> I.Body () -> ItemEmit a
runImpBodyEmit = flip withImpBody
{-# INLINE runImpBodyEmit #-}

withModule
  :: Assembly (H.Module ()) -> H.Module () -> ModuleEmit a -> CompilePure a
withModule a m f = runModuleEmit f a m
{-# INLINE withModule #-}

withItem :: H.Item () -> ItemEmit a -> ModuleEmit a
withItem i = withReaderT $ \(a, m) -> (a, m, i)
{-# INLINE withItem #-}

withBody :: H.Body () -> BodyEmit a -> ItemEmit a
withBody b = withReaderT $ \(a, m, i) -> (a, m, i, b)
{-# INLINE withBody #-}

withImpBody :: I.Body () -> ImpBodyEmit a -> ItemEmit a
withImpBody b = withReaderT $ \(a, m, i) -> (a, m, i, b)
{-# INLINE withImpBody #-}

class Monad m => MonadModuleEmit (m :: * -> *) where
  askAssembly :: m (Assembly (H.Module ()))
  askModule   :: m (H.Module ())

class MonadModuleEmit m => MonadItemEmit (m :: * -> *) where
  askItem :: m (H.Item ())

class MonadItemEmit m => MonadBodyEmit (m :: * -> *) where
  askBody :: m (H.Body ())

class MonadItemEmit m => MonadImpBodyEmit (m :: * -> *) where
  askImpBody :: m (I.Body ())

instance MonadModuleEmit ModuleEmit where
  askAssembly = view _1
  askModule   = view _2
  {-# INLINE askModule #-}

instance MonadModuleEmit ItemEmit where
  askAssembly = view _1
  askModule   = view _2
  {-# INLINE askModule #-}

instance MonadModuleEmit BodyEmit where
  askAssembly = view _1
  askModule   = view _2
  {-# INLINE askModule #-}

instance MonadModuleEmit ImpBodyEmit where
  askAssembly = view _1
  askModule   = view _2
  {-# INLINE askModule #-}

instance MonadItemEmit ItemEmit where
  askItem = view _3
  {-# INLINE askItem #-}

instance MonadItemEmit BodyEmit where
  askItem = view _3
  {-# INLINE askItem #-}

instance MonadItemEmit ImpBodyEmit where
  askItem = view _3
  {-# INLINE askItem #-}

instance MonadBodyEmit BodyEmit where
  askBody = view _4
  {-# INLINE askBody #-}

instance MonadImpBodyEmit ImpBodyEmit where
  askImpBody = view _4
  {-# INLINE askImpBody #-}
