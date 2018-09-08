module Intentio.Codegen.Emitter.Printer
  ( printCAssembly
  , printCAssemblyToDirectory
  , printCAssemblyToWorkDir
  , printCModule
  , hPrintCModule
  , printCModule'
  , hPrintCModule'
  )
where

import           Intentio.Prelude

import           System.FilePath                ( (</>) )
import           Text.PrettyPrint.Mainland      ( putDocLn
                                                , hPutDocLn
                                                )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )

import   Intentio.Cache.WorkDir (getWorkDir)
import           Intentio.Compiler              ( Assembly
                                                , mapModulesM_
                                                , Compile
                                                , liftIOE
                                                )

import           Intentio.Codegen.Emitter.Types ( CModuleDef
                                                , cModuleDefDefinitions
                                                , cModuleDefFileName
                                                )

printCAssembly :: Assembly (CModuleDef t) -> Compile ()
printCAssembly = mapModulesM_ printCModule

printCAssemblyToDirectory :: FilePath -> Assembly (CModuleDef t) -> Compile ()
printCAssemblyToDirectory dir = mapModulesM_ f
 where
  f modul = do
    let path = dir </> modul ^. cModuleDefFileName
    liftIOE $ withFile path WriteMode (flip hPrintCModule' modul)

printCAssemblyToWorkDir :: Assembly (CModuleDef t) -> Compile ()
printCAssemblyToWorkDir asm = do
  wd <- getWorkDir "c"
  printCAssemblyToDirectory wd asm

printCModule :: CModuleDef t -> Compile ()
printCModule = liftIOE . printCModule'

printCModule' :: CModuleDef t -> IO ()
printCModule' m = putDocLn . ppr $ m ^. cModuleDefDefinitions

hPrintCModule :: Handle -> CModuleDef t -> Compile ()
hPrintCModule h = liftIOE . hPrintCModule' h

hPrintCModule' :: Handle -> CModuleDef t -> IO ()
hPrintCModule' h m = hPutDocLn h . ppr $ m ^. cModuleDefDefinitions
