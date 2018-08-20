module Intentio.Driver where

import           Intentio.Prelude

import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )
import           Text.Pretty.Simple             ( pPrint )

import           Intentio.Compiler              ( Assembly
                                                , Module
                                                , Compile
                                                , liftIOE
                                                , compileDiagnostics
                                                , runCompileFresh
                                                )
import           Intentio.Diagnostics           ( diagnosticShow )
import           Language.Intentio.Compiler     ( SourceFile
                                                , parseSourceFiles
                                                )

import           Intentio.Driver.Opts           ( buildInputAssembly )

--------------------------------------------------------------------------------
-- Compiler pipeline

compilerPipeline :: Assembly SourceFile -> Compile ()
compilerPipeline = parseSourceFiles >=> traceStep_

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
traceStep_ = pPrint
