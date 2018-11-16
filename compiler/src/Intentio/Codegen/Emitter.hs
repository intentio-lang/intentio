{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter
  ( emitCAssembly
  , emitModuleHeader
  , emitModuleSource
  , module Intentio.Codegen.Emitter.Printer
  , module Intentio.Codegen.Emitter.Types
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cunit )

import           Intentio.Codegen.Emitter.Module
                                                ( emitModuleHeader
                                                , emitModuleSource
                                                , buildHeaderComment
                                                )
import           Intentio.Codegen.Emitter.Printer
import           Intentio.Codegen.SymbolNames   ( cItemName'
                                                , cModuleFileName
                                                )
import           Intentio.Codegen.Emitter.Types
import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , ItemName(..)
                                                , ModuleName(..)
                                                , assemblyMainModuleName
                                                , assemblyModules
                                                , concatMapModulesM
                                                , moduleName
                                                )
import           Intentio.Diagnostics           ( sourcePos )
import qualified Intentio.Hir                  as H

mainFileName :: FilePath
mainFileName = "intentio_main.c"

emitCAssembly
  :: Assembly (H.Module ()) -> CompilePure (Assembly (CModuleDef Void))
emitCAssembly = fmap addMain . concatMapModulesM emit
 where
  emit modul = do
    header <- cModuleEraseType <$> emitModuleHeader modul
    source <- cModuleEraseType <$> emitModuleSource modul
    return [header, source]

  addMain asm = case asm ^. assemblyMainModuleName of
    Nothing -> asm
    Just origMain ->
      let main = buildMain origMain
      in  asm
          & (assemblyModules . at (main ^. moduleName) ?~ main)
          & (assemblyMainModuleName ?~ main ^. moduleName)

  buildMain origMain = CModuleDef
    { _cModuleDefSourcePos    = () ^. sourcePos
    , _cModuleDefIntentioName = ModuleName "%intentio_main"
    , _cModuleDefFileName     = mainFileName
    , _cModuleDefDefinitions  = mainUnit origMain
    }

-- brittany-disable-next-binding
mainUnit :: ModuleName -> [C.Definition]
mainUnit mName = [cunit|
    $esc:headerText

    $esc:("#include <stdlib.h>")
    $esc:("#include <stdio.h>")
    $esc:("#include <intentio.h>")
    $esc:includeMainMod

    int main () {
      typename IeoResult r = $id:mainFn ();
      if (r.succ) {
        return EXIT_SUCCESS;
      } else {
        fprintf(stderr, "Intentio program failed unexpectedly.\n");
        return EXIT_FAILURE;
      }
    }
  |]
  where
    headerText     = buildHeaderComment mainFileName mName
    mainModHeader  = cModuleFileName @CModuleHeader mName
    includeMainMod = "#include \"" <> mainModHeader <> "\""
    mainFn         = cItemName' mName (ItemName "main")
