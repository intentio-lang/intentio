module Intentio.Codegen.Emitter
  ( emitCAssembly
  , emitModuleHeader
  , emitModuleSource
  , module Intentio.Codegen.Emitter.Printer
  , module Intentio.Codegen.Emitter.Types
  )
where

import           Intentio.Prelude

import           Intentio.Codegen.Emitter.Module
                                                ( emitModuleHeader
                                                , emitModuleSource
                                                )
import           Intentio.Codegen.Emitter.Printer
import           Intentio.Codegen.SymbolNames   ( cModuleFileName )
import           Intentio.Codegen.Emitter.Types
import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , ModuleName(..)
                                                , assemblyMainModuleName
                                                , concatMapModulesM
                                                )
import qualified Intentio.Hir                  as H

emitCAssembly
  :: Assembly (H.Module ()) -> CompilePure (Assembly (CModuleDef Void))
emitCAssembly = fmap addMainName . concatMapModulesM emit
 where
  addMainName asm = asm & (assemblyMainModuleName %~ fmap mkMainName)

  mkMainName = ModuleName . toS . cModuleFileName @CModuleSource

  emit modul = do
    header <- cModuleEraseType <$> emitModuleHeader modul
    source <- cModuleEraseType <$> emitModuleSource modul
    return [header, source]
