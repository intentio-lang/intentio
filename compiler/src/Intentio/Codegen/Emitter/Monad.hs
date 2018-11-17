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
import           Intentio.Compiler              ( CompilePure )
import qualified Intentio.Hir                  as H

type Emit r = ReaderT r CompilePure

type ModuleEmit = Emit ModuleContext
type ItemEmit = Emit ItemContext
type BodyEmit = Emit BodyContext
type ImpBodyEmit = Emit ImpBodyContext

type ModuleContext = H.Module ()
type ItemContext = (H.Module (), H.Item ())
type BodyContext = (H.Module (), H.Item (), H.Body ())
type ImpBodyContext = (H.Module (), H.Item (), I.Body ())

runModuleEmit :: ModuleEmit a -> H.Module () -> CompilePure a
runModuleEmit = runReaderT
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

withModule :: H.Module () -> ModuleEmit a -> CompilePure a
withModule = flip runModuleEmit
{-# INLINE withModule #-}

withItem :: H.Item () -> ItemEmit a -> ModuleEmit a
withItem i = withReaderT (, i)
{-# INLINE withItem #-}

withBody :: H.Body () -> BodyEmit a -> ItemEmit a
withBody b = withReaderT $ \(m, i) -> (m, i, b)
{-# INLINE withBody #-}

withImpBody :: I.Body () -> ImpBodyEmit a -> ItemEmit a
withImpBody b = withReaderT $ \(m, i) -> (m, i, b)
{-# INLINE withImpBody #-}

class Monad m => MonadModuleEmit (m :: * -> *) where
  askModule :: m (H.Module ())

class MonadModuleEmit m => MonadItemEmit (m :: * -> *) where
  askItem :: m (H.Item ())

class MonadItemEmit m => MonadBodyEmit (m :: * -> *) where
  askBody :: m (H.Body ())

class MonadItemEmit m => MonadImpBodyEmit (m :: * -> *) where
  askImpBody :: m (I.Body ())

instance MonadModuleEmit ModuleEmit where
  askModule = ask
  {-# INLINE askModule #-}

instance MonadModuleEmit ItemEmit where
  askModule = view _1
  {-# INLINE askModule #-}

instance MonadModuleEmit BodyEmit where
  askModule = view _1
  {-# INLINE askModule #-}

instance MonadModuleEmit ImpBodyEmit where
  askModule = view _1
  {-# INLINE askModule #-}

instance MonadItemEmit ItemEmit where
  askItem = view _2
  {-# INLINE askItem #-}

instance MonadItemEmit BodyEmit where
  askItem = view _2
  {-# INLINE askItem #-}

instance MonadItemEmit ImpBodyEmit where
  askItem = view _2
  {-# INLINE askItem #-}

instance MonadBodyEmit BodyEmit where
  askBody = view _3
  {-# INLINE askBody #-}

instance MonadImpBodyEmit ImpBodyEmit where
  askImpBody = view _3
  {-# INLINE askImpBody #-}
