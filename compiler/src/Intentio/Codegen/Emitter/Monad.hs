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
  , WT
  , execWT
  , tellWT
  , tellsWT
  , subWT
  )
where

import           Intentio.Prelude

import qualified Data.List                     as List

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

type WT w m a = StateT [[w]] m a

execWT :: Monad m => WT w m a -> m [w]
execWT = fmap (reverse . List.head) . flip execStateT [[]]
{-# INLINE execWT #-}

tellWT :: Monad m => w -> WT w m ()
tellWT w' = modify $ \(w : ws) -> (w' : w) : ws
{-# INLINE tellWT #-}

tellsWT :: Monad m => [w] -> WT w m ()
tellsWT = mapM_ tellWT
{-# INLINE tellsWT #-}

subWT :: Monad m => WT w m a -> WT w m [w]
subWT f = modify ([] :) >> f >> state (\(w : ws) -> (w, ws))
{-# INLINE subWT #-}

instance MonadModuleEmit m => MonadModuleEmit (StateT w m) where
  askModule = lift askModule
  {-# INLINE askModule #-}

instance MonadItemEmit m => MonadItemEmit (StateT w m) where
  askItem = lift askItem
  {-# INLINE askItem #-}

instance MonadBodyEmit m => MonadBodyEmit (StateT w m) where
  askBody = lift askBody
  {-# INLINE askBody #-}

instance MonadImpBodyEmit m => MonadImpBodyEmit (StateT w m) where
  askImpBody = lift askImpBody
  {-# INLINE askImpBody #-}
