{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Module
  ( emitModuleHeader
  , emitModuleSource
  , emitModuleHeader'
  , emitModuleSource'
  )
where

import           Intentio.Prelude

import           Data.HashSet.Lens              ( setmapped )
import qualified Data.Text                     as T
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cunit )
import           NeatInterpolation              ( text )

import           Intentio.Codegen.Emitter.Item  ( emitItemHeader
                                                , emitItemSource
                                                )
import           Intentio.Codegen.Emitter.Monad ( ModuleEmit
                                                , askModule
                                                , runModuleEmit
                                                , runItemEmit
                                                )
import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                , CModuleDef(..)
                                                )
import           Intentio.Codegen.Emitter.Util  ( getItemById )
import           Intentio.Codegen.SymbolNames   ( GetCModuleFileName(..) )
import           Intentio.Compiler              ( CompilePure )
import qualified Intentio.Hir                  as H

emitModuleHeader :: H.Module () -> CompilePure (CModuleDef CModuleHeader)
emitModuleHeader = runModuleEmit emitModuleHeader'

emitModuleSource :: H.Module () -> CompilePure (CModuleDef CModuleSource)
emitModuleSource = runModuleEmit emitModuleSource'

emitModuleHeader' :: ModuleEmit (CModuleDef CModuleHeader)
emitModuleHeader' = emitModule

emitModuleSource' :: ModuleEmit (CModuleDef CModuleSource)
emitModuleSource' = emitModule

emitModule
  :: forall t
   . (GetCModuleFileName t, ModuleEmitter t)
  => ModuleEmit (CModuleDef t)
emitModule = do
  _cModuleDefSourcePos    <- view H.moduleSourcePos
  _cModuleDefIntentioName <- view H.moduleName
  _cModuleDefFileName     <- cModuleFileName @t <$> view H.moduleName
  imports                 <- emitImports @t
  defs                    <- emitItems @t

  let headerText =
        toS $ buildHeaderComment _cModuleDefFileName _cModuleDefIntentioName

  let _cModuleDefDefinitions = concat
        [ [cunit| $esc:headerText |]
        , [cunit| $esc:("#include <intentio.h>") |]
        , imports
        , defs
        ]

  return CModuleDef { .. }

class ModuleEmitter t where
  emitImports :: ModuleEmit [C.Definition]
  emitItems :: ModuleEmit [C.Definition]

instance ModuleEmitter CModuleHeader where
  emitImports = pure []

  emitItems = view H.moduleItemIds
    >>= mapM (getItemById >=> runItemEmit emitItemHeader)
    <&> concat

instance ModuleEmitter CModuleSource where
  emitImports =
    concatMap (\t -> [cunit| $esc:t |])
      .   toList
      .   (setmapped %~ inc . fst)
      .   view H.moduleImports
      <$> askModule
    where inc n = "#include \"" <> cModuleFileName @CModuleHeader n <> "\""

  emitItems = do
    items <- view H.moduleItemIds >>= mapM getItemById
    hs <- forM items $ runItemEmit emitItemHeader
    ss <- forM items $ runItemEmit emitItemSource
    return $ concat (hs <> ss)

-- brittany-disable-next-binding
buildHeaderComment :: FilePath -> H.ModuleName -> Text
buildHeaderComment fileName (H.ModuleName moduleName) = [text|
    //$line
    // Generated by intentioc. DO NOT MODIFY!
    // File:   $fileNameS
    // Module: $moduleName
    //$line
  |]
  where
    line      = T.replicate 77 "-"
    fileNameS = toS fileName
