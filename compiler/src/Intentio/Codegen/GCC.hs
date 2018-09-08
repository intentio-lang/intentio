module Intentio.Codegen.GCC
  ( runGCC
  )
where

import           Intentio.Prelude

import           System.Exit                    ( ExitCode(..) )
import           System.FilePath                ( (-<.>)
                                                , isExtensionOf
                                                , replaceDirectory
                                                )
import qualified System.Process.Typed          as P

import           Intentio.Cache.WorkDir         ( getWorkDir )
import           Intentio.Compiler              ( Assembly
                                                , AssemblyType(..)
                                                , Compile
                                                , assemblyType
                                                , assemblyModules
                                                , assemblyOutputPath
                                                , pushDiagnostic
                                                )
import           Intentio.Diagnostics           ( SourcePos(..)
                                                , cerror
                                                )

import           Intentio.Codegen.Emitter.Types ( CFile(..)
                                                , cFilePath
                                                )

runGCC :: Assembly CFile -> Compile ()
runGCC asm = do
  let gccExec    = "gcc"
  let runGCCProc = runProc "GCC" gccExec

  let arExec     = "ar"
  let runARProc  = runProc "AR" arExec

  objwd <- getWorkDir "obj"

  let getObjFile f = f `replaceDirectory` objwd -<.> ".o"

  getFilesByExt ".c" `forM_` \f -> do
    let o = getObjFile f
    runGCCProc o ["-c", "-o", o, f]

  let objFiles = getObjFile <$> getFilesByExt ".c"
  let outFile  = asm ^. assemblyOutputPath

  case asm ^. assemblyType of
    Library -> runARProc outFile (["rcs", outFile] <> objFiles)
    Program -> runGCCProc outFile (["-o", outFile] <> objFiles)
 where
  getFilesByExt ext =
    asm ^. assemblyModules <&> view cFilePath & toList & filter
      (isExtensionOf ext)

runProc :: Text -> FilePath -> FilePath -> [String] -> Compile ()
runProc procName exec errFile args = P.runProcess conf >>= checkExitCode
 where
  checkExitCode ExitSuccess     = return ()
  checkExitCode (ExitFailure e) = pushDiagnostic $ cerror
    (SourcePos errFile 0 0)
    ("Running " <> procName <> " failed with exit code: " <> show e)

  conf =
    P.proc exec args
      & P.setStdin P.closed
      & P.setStdout P.inherit
      & P.setStderr P.inherit
      & P.setWorkingDirInherit
      & P.setEnvInherit
