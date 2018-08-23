module Intentio.Hir.Compiler
  ( readHirDumpFiles
  , readHirDumpFile
  , readHirDumpTexts
  , readHirDumpText
  )
where

import           Intentio.Prelude

import           Data.Aeson                     ( decodeStrict'
                                                , decodeFileStrict'
                                                )

import           Intentio.Compiler              ( Assembly
                                                , mapModulesM
                                                , Compile
                                                , CompilePure
                                                , liftIOE
                                                , pushDiagnostic
                                                )
import           Intentio.Diagnostics           ( cerrorFor )
import           Language.Intentio.Compiler     ( SourceFile(..)
                                                , SourceText(..)
                                                , sourceFilePath
                                                , sourceTextContent
                                                )

import           Intentio.Hir                   ( Module )

readHirDumpFiles :: Assembly SourceFile -> Compile (Assembly Module)
readHirDumpFiles = mapModulesM readHirDumpFile

readHirDumpFile :: SourceFile -> Compile Module
readHirDumpFile f = liftIOE (decodeFileStrict' (f ^. sourceFilePath)) >>= \case
  Just m  -> return m
  Nothing -> pushDiagnostic (cerrorFor f "Invalid HIR dump") >> unreachable

readHirDumpTexts :: Assembly SourceText -> CompilePure (Assembly Module)
readHirDumpTexts = mapModulesM readHirDumpText

readHirDumpText :: SourceText -> CompilePure Module
readHirDumpText t = case decodeStrict' (t ^. sourceTextContent & toS) of
  Just m  -> return m
  Nothing -> pushDiagnostic (cerrorFor t "Invalid HIR dump") >> unreachable
