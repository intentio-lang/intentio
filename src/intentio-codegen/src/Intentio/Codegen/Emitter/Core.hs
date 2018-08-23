{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Core
  ( emitCAssembly
  , emitCModuleHeader
  , emitCModuleSource
  , emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cunit )

import           Intentio.Compiler              ( ModuleName(..)
                                                , Assembly
                                                , assemblyMainModuleName
                                                , concatMapModulesM
                                                , CompilePure
                                                , pushIceFor
                                                )
import qualified Intentio.Hir                  as H

import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                , CModuleDef(..)
                                                , cModuleEraseType
                                                )
import           Intentio.Codegen.SymbolNames   ( GetCModuleFileName(..) )

--------------------------------------------------------------------------------
-- Emitter monad

type Emit r = ReaderT r CompilePure
type MEmit = Emit H.Module

--------------------------------------------------------------------------------
-- Emitter entry points

emitCAssembly :: Assembly H.Module -> CompilePure (Assembly (CModuleDef Void))
emitCAssembly = fmap addMainName . concatMapModulesM emit
 where
  addMainName asm = asm & (assemblyMainModuleName %~ fmap mkMainName)

  mkMainName = ModuleName . toS . cModuleFileName @CModuleSource

  emit modul = do
    header <- cModuleEraseType <$> emitCModuleHeader modul
    source <- cModuleEraseType <$> emitCModuleSource modul
    return [header, source]

emitCModuleHeader :: H.Module -> CompilePure (CModuleDef CModuleHeader)
emitCModuleHeader = runReaderT (emitCModule' emitItemHeader')

emitCModuleSource :: H.Module -> CompilePure (CModuleDef CModuleSource)
emitCModuleSource = runReaderT (emitCModule' emitItemSource')

emitItemHeader :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemHeader modul itemId = runReaderT (emitItemHeader' itemId) modul

emitItemSource :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemSource modul itemId = runReaderT (emitItemSource' itemId) modul

--------------------------------------------------------------------------------
-- Header emitter functions

emitItemHeader' :: H.ItemId -> MEmit [C.Definition]
emitItemHeader' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem _ _ -> return []
    H.FnItem bodyId  -> emitFnHeader item bodyId

emitFnHeader :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnHeader _ _ = return [cunit| int f(int x); |]

--------------------------------------------------------------------------------
-- Source emitter functions

emitItemSource' :: H.ItemId -> MEmit [C.Definition]
emitItemSource' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem modName _ -> emitImportItem modName
    H.FnItem bodyId        -> emitFnItem item bodyId

emitImportItem :: ModuleName -> MEmit [C.Definition]
emitImportItem modName = return [cunit| $esc:f |]
  where f = "#include \"" <> cModuleFileName @CModuleHeader modName <> "\""

emitFnItem :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnItem _ _ = return [cunit| int f(int x) { return x; } |]

--------------------------------------------------------------------------------
-- Shared emitter functions

--------------------------------------------------------------------------------
-- Helpers

emitCModule'
  :: forall t
   . GetCModuleFileName t
  => (H.ItemId -> MEmit [C.Definition])
  -> MEmit (CModuleDef t)
emitCModule' f = do
  _cModuleDefSourcePos    <- view H.moduleSourcePos
  _cModuleDefIntentioName <- view H.moduleName
  _cModuleDefFileName     <- cModuleFileName @t <$> view H.moduleName
  ids                     <- view H.moduleItemIds
  _cModuleDefDefinitions  <- concat <$> mapM f ids
  return CModuleDef {..}

getItemById :: H.ItemId -> MEmit H.Item
getItemById itemId = do
  modul <- ask
  case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> lift $ pushIceFor modul $ "Unknown item " <> show itemId
