{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Core
  ( emitCModuleHeader
  , emitCModuleSource
  , emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cunit )

import           Intentio.Compiler              ( ModuleName(..)
                                                , CompilePure
                                                , pushIceFor
                                                )
import qualified Intentio.Hir                  as H

import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                , CModuleDef(..)
                                                )
import           Intentio.Codegen.SymbolNames   ( moduleFileNameH )

--------------------------------------------------------------------------------
-- Emitter monad

type Emit r = ReaderT r CompilePure
type MEmit = Emit H.Module

--------------------------------------------------------------------------------
-- Emitter entry points

emitCModuleHeader :: H.Module -> CompilePure (CModuleDef CModuleHeader)
emitCModuleHeader = runReaderT (emitCModule' emitItemHeader')

emitCModuleSource :: H.Module -> CompilePure (CModuleDef CModuleSource)
emitCModuleSource = runReaderT (emitCModule' emitItemSource')

emitItemHeader :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemHeader modul itemId = runReaderT (emitItemHeader' itemId) modul

emitItemSource :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemSource modul itemId = runReaderT (emitItemSource' itemId) modul

--------------------------------------------------------------------------------
-- Emitter functions

emitItemHeader' :: H.ItemId -> MEmit [C.Definition]
emitItemHeader' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem _ _ -> return []
    H.FnItem bodyId  -> emitFnHeader item bodyId

emitItemSource' :: H.ItemId -> MEmit [C.Definition]
emitItemSource' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem modName _ -> emitImportItem modName
    H.FnItem bodyId        -> emitFnItem item bodyId

emitImportItem :: ModuleName -> MEmit [C.Definition]
emitImportItem modName = return [cunit| $esc:("#include " <> f) |]
  where f = "\"" <> moduleFileNameH modName <> "\""

emitFnHeader :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnHeader _ _ = return [cunit| int f(int x); |]

emitFnItem :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnItem _ _ = return [cunit| int f(int x) { return x; } |]

--------------------------------------------------------------------------------
-- Helpers

emitCModule' :: (H.ItemId -> MEmit [C.Definition]) -> MEmit (CModuleDef t)
emitCModule' f = do
  _cModuleDefSourcePos   <- view H.moduleSourcePos
  _cModuleDefName        <- view H.moduleName
  ids                    <- view H.moduleItemIds
  _cModuleDefDefinitions <- concat <$> mapM f ids
  return CModuleDef {..}

getItemById :: H.ItemId -> MEmit H.Item
getItemById itemId = do
  modul <- ask
  case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> lift $ pushIceFor modul $ "Unknown item " <> show itemId
