module Intentioc where

import           Intentio.Prelude

import qualified Data.Aeson
import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )

import           Intentio.Codegen               ( emitCAssembly
                                                , printCAssemblyToWorkDir
                                                , runGCC
                                                )
import           Intentio.Compiler              ( Assembly
                                                , Module
                                                , Compile
                                                , impurify
                                                , compileDiagnostics
                                                , runCompileFresh
                                                )
import           Intentio.Compiler.ModulePath   ( injectStd
                                                , resolveModuleFiles
                                                )
import           Intentio.Diagnostics           ( diagnosticShow )
import           Intentio.Hir.Lowering          ( lowerAssembly )
import           Intentio.Resolver              ( resolveAssembly )
import           Language.Intentio.Compiler     ( SourceFile
                                                , parseSourceFiles
                                                )

import           Intentioc.Opts                 ( buildInputAssembly )

--------------------------------------------------------------------------------
-- Compiler pipeline

compilerPipeline :: Assembly SourceFile -> Compile ()
compilerPipeline =
  (return . injectStd)
    >=> resolveModuleFiles
    >=> parseSourceFiles
    >=> (impurify . resolveAssembly)
    >=> (impurify . lowerAssembly)
    >=> (impurify . emitCAssembly)
    >=> printCAssemblyToWorkDir
    >=> runGCC

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
runCompiler = buildInputAssembly >>= compilerPipeline

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
