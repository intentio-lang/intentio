{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter
  ( module Intentio.Codegen.Emitter.Types
  , emitCModuleHeader
  , emitCModuleSource
  , emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cunit )
import qualified Text.PrettyPrint.Mainland     as PP

import           Intentio.Compiler              ( ModuleName(..)
                                                , CompilePure
                                                , pushIceFor
                                                )
import qualified Intentio.Hir                  as H

import           Intentio.Codegen.Emitter.Types
import           Intentio.Codegen.SymbolNames   ( moduleFileNameH )

emitCModuleHeader :: H.Module -> CompilePure (CModuleDef CModuleHeader)
emitCModuleHeader = doEmitCModule emitItemHeader

emitItemHeader :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemHeader modul itemId = do
  item <- case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> pushIceFor modul $ "Unknown item " <> show itemId
  case item ^. H.itemKind of
    H.ImportItem _ _ -> return []
    H.FnItem bodyId  -> emitFnHeader modul item bodyId

emitCModuleSource :: H.Module -> CompilePure (CModuleDef CModuleSource)
emitCModuleSource = doEmitCModule emitItemSource

doEmitCModule
  :: Monad m
  => (H.Module -> H.ItemId -> m [C.Definition])
  -> H.Module
  -> m (CModuleDef t)
doEmitCModule f m = do
  let _cModuleDefSourcePos = m ^. H.moduleSourcePos
  let _cModuleDefName      = m ^. H.moduleName
  _cModuleDefDefinitions <- concat <$> mapM (f m) (m ^. H.moduleItemIds)
  return CModuleDef {..}

emitItemSource :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemSource modul itemId = do
  item <- case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> pushIceFor modul $ "Unknown item " <> show itemId
  case item ^. H.itemKind of
    H.ImportItem modName _ -> emitImportItem modName
    H.FnItem bodyId        -> emitFnItem modul item bodyId

emitImportItem :: ModuleName -> CompilePure [C.Definition]
emitImportItem modName = return [cunit| $esc:("#include " <> f) |]
  where f = "\"" <> moduleFileNameH modName <> "\""

emitFnHeader :: H.Module -> H.Item -> H.BodyId -> CompilePure [C.Definition]
emitFnHeader _ _ _ = return [cunit| int f(int x); |]

emitFnItem :: H.Module -> H.Item -> H.BodyId -> CompilePure [C.Definition]
emitFnItem _ _ _ = return [cunit| int f(int x) { return x; } |]
