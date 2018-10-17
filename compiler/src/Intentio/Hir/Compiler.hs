module Intentio.Hir.Compiler
  ( readHirDumpFiles
  , readHirDumpFile
  , readHirDumpTexts
  , readHirDumpText
  )
where

import           Intentio.Prelude

import           Data.Aeson                     ( eitherDecodeStrict'
                                                , eitherDecodeFileStrict'
                                                )

import           Intentio.Compiler              ( Assembly
                                                , mapModulesM
                                                , Compile
                                                , CompilePure
                                                , CompileT
                                                , liftIOE
                                                , pushDiagnostics
                                                )
import           Intentio.Diagnostics           ( HasSourcePos
                                                , cerrorFor
                                                , cnoteFor
                                                )
import           Language.Intentio.Compiler     ( SourceFile(..)
                                                , SourceText(..)
                                                , sourceFilePath
                                                , sourceTextContent
                                                )

import           Intentio.Hir                   ( Module )

readHirDumpFiles :: Assembly SourceFile -> Compile (Assembly (Module ()))
readHirDumpFiles = mapModulesM readHirDumpFile

readHirDumpFile :: SourceFile -> Compile (Module ())
readHirDumpFile f =
  liftIOE (eitherDecodeFileStrict' (f ^. sourceFilePath)) >>= \case
    Right m -> return m
    Left  e -> err f e

readHirDumpTexts :: Assembly SourceText -> CompilePure (Assembly (Module ()))
readHirDumpTexts = mapModulesM readHirDumpText

readHirDumpText :: SourceText -> CompilePure (Module ())
readHirDumpText t = case eitherDecodeStrict' (t ^. sourceTextContent & toS) of
  Right m -> return m
  Left  e -> err t e

err :: (HasSourcePos t, Monad m) => t -> String -> CompileT m a
err t e = do
  pushDiagnostics [cerrorFor t "Invalid HIR dump.", cnoteFor t $ toS e]
  unreachable
