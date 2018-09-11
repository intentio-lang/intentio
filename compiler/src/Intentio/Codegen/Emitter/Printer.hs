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

import           Intentio.Cache.WorkDir         ( getWorkDir )
import           Intentio.Compiler              ( Assembly
                                                , mapModulesM
                                                , mapModulesM_
                                                , Compile
                                                , liftIOE
                                                )

import           Intentio.Codegen.Emitter.Types ( CModuleDef
                                                , CFile(..)
                                                , cModuleDefDefinitions
                                                , cModuleDefFileName
                                                )

printCAssembly :: Assembly (CModuleDef t) -> Compile ()
printCAssembly = mapModulesM_ printCModule

printCAssemblyToDirectory
  :: FilePath -> Assembly (CModuleDef t) -> Compile (Assembly CFile)
printCAssemblyToDirectory dir = mapModulesM f
 where
  f modul = do
    let path = dir </> modul ^. cModuleDefFileName
    liftIOE $ withFile path WriteMode (flip hPrintCModule' modul)
    return $ CFile path

printCAssemblyToWorkDir :: Assembly (CModuleDef t) -> Compile (Assembly CFile)
printCAssemblyToWorkDir asm =
  getWorkDir "cgen" >>= flip printCAssemblyToDirectory asm

printCModule :: CModuleDef t -> Compile ()
printCModule = liftIOE . printCModule'

printCModule' :: CModuleDef t -> IO ()
printCModule' m = putDocLn . ppr $ m ^. cModuleDefDefinitions

hPrintCModule :: Handle -> CModuleDef t -> Compile ()
hPrintCModule h = liftIOE . hPrintCModule' h

hPrintCModule' :: Handle -> CModuleDef t -> IO ()
hPrintCModule' h m = hPutDocLn h . ppr $ m ^. cModuleDefDefinitions
