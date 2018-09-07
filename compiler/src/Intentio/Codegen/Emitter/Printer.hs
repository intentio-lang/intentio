module Intentio.Codegen.Emitter.Printer
  ( printCAssembly
  , printCModule
  , hPrintCModule
  , printCModule'
  , hPrintCModule'
  )
where

import           Intentio.Prelude

import           Text.PrettyPrint.Mainland      ( putDocLn
                                                , hPutDocLn
                                                )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )

import           Intentio.Compiler              ( Assembly
                                                , mapModulesM_
                                                , Compile
                                                , liftIOE
                                                )

import           Intentio.Codegen.Emitter.Types ( CModuleDef
                                                , cModuleDefDefinitions
                                                )

printCAssembly :: Assembly (CModuleDef t) -> Compile ()
printCAssembly = mapModulesM_ printCModule

printCModule :: CModuleDef t -> Compile ()
printCModule = liftIOE . printCModule'

printCModule' :: CModuleDef t -> IO ()
printCModule' m = putDocLn . ppr $ m ^. cModuleDefDefinitions

hPrintCModule :: Handle -> CModuleDef t -> Compile ()
hPrintCModule h = liftIOE . hPrintCModule' h

hPrintCModule' :: Handle -> CModuleDef t -> IO ()
hPrintCModule' h m = hPutDocLn h . ppr $ m ^. cModuleDefDefinitions
