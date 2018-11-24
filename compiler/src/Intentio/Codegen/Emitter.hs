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
import           Intentio.Codegen.SymbolNames   ( cItemName
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
                                                , lookupMainModule
                                                , moduleResolveItem
                                                )
import           Intentio.Diagnostics           ( sourcePos )
import qualified Intentio.Hir                  as H

mainFileName :: FilePath
mainFileName = "intentio_main.c"

emitCAssembly
  :: Assembly (H.Module ()) -> CompilePure (Assembly (CModuleDef Void))
emitCAssembly asmH = do
  asmC <- concatMapModulesM emit asmH
  case lookupMainModule asmH of
    Nothing    -> return asmC
    Just mainH -> do
      mainFnH <- moduleResolveItem (ItemName "main") mainH
      let mainC = CModuleDef
            { _cModuleDefSourcePos    = mainH ^. sourcePos
            , _cModuleDefIntentioName = ModuleName "%intentio_main"
            , _cModuleDefFileName     = mainFileName
            , _cModuleDefDefinitions  = mainUnit mainH mainFnH
            }
      return
        $ asmC
        & (assemblyModules . at (mainC ^. moduleName) ?~ mainC)
        & (assemblyMainModuleName ?~ mainC ^. moduleName)
 where
  emit modH = do
    header <- cModuleEraseType <$> emitModuleHeader asmH modH
    source <- cModuleEraseType <$> emitModuleSource asmH modH
    return [header, source]

-- brittany-disable-next-binding
mainUnit :: H.Module () -> H.Item () -> [C.Definition]
mainUnit modul item = [cunit|
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
        typename IeoResult s = ieo_str(r.term);
        if(s.succ) {
          fprintf(stderr, "Intentio program failed unexpectedly: %s\n",
                  ieo_string_c_str(s.term));
        } else {
          fprintf(stderr, "Intentio program failed unexpectedly.\n");
        }
        return EXIT_FAILURE;
      }
    }
  |]
  where
    headerText     = buildHeaderComment mainFileName (modul ^. moduleName)
    mainModHeader  = cModuleFileName @CModuleHeader (modul ^. moduleName)
    includeMainMod = "#include \"" <> mainModHeader <> "\""
    mainFn         = cItemName modul item
