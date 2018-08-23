module Intentio.Driver where

import           Intentio.Prelude

import qualified Data.Aeson
import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )

import           Intentio.Codegen               ( emitCAssembly
                                                , printCAssembly
                                                )
import           Intentio.Compiler              ( Assembly
                                                , Module
                                                , Compile
                                                , impurify
                                                , liftIOE
                                                , compileDiagnostics
                                                , runCompileFresh
                                                )
import           Intentio.Diagnostics           ( diagnosticShow )
import           Intentio.Hir.Compiler          ( readHirDumpFiles )
import           Language.Intentio.Compiler     ( SourceFile
                                                , parseSourceFiles
                                                )

import           Intentio.Driver.Opts           ( buildInputAssembly )

--------------------------------------------------------------------------------
-- Compiler pipeline

compilerPipeline :: Assembly SourceFile -> Compile ()
-- compilerPipeline = parseSourceFiles >=> traceStep_
compilerPipeline =
  readHirDumpFiles >=> (impurify . emitCAssembly) >=> printCAssembly

--------------------------------------------------------------------------------
-- Main function

runDriver :: IO ()
runDriver = do
  (res, ctx) <- runCompileFresh runCompiler
  ctx ^. compileDiagnostics & putText . diagnosticShow
  case res of
    Just () -> exitSuccess
    Nothing -> exitFailure

runCompiler :: Compile ()
runCompiler = liftIOE buildInputAssembly >>= compilerPipeline

--------------------------------------------------------------------------------
-- Helpers

traceStep :: (Module a, Show a) => Assembly a -> Compile (Assembly a)
traceStep a = traceStep_ a >> return a

traceStep_ :: (Module a, Show a) => Assembly a -> Compile ()
traceStep_ = putText . show

traceStepJSON :: (Module a, ToJSON a) => Assembly a -> Compile (Assembly a)
traceStepJSON a = traceStepJSON_ a >> return a

traceStepJSON_ :: (Module a, ToJSON a) => Assembly a -> Compile ()
traceStepJSON_ = putStrLn . Data.Aeson.encode
