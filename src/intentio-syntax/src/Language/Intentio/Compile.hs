module Language.Intentio.Compile
  ( SourceFile(..)
  , sourceFilePath
  , parseSourceFileSet
  , readSourceFile
  , parseSourceFile
  , parseSourceText
  )
where

import           Intentio.Prelude

import           Control.Concurrent.Async       ( mapConcurrently )
import           Data.List.NonEmpty             ( fromList )
import           Data.Text.IO                   ( readFile )
import           System.FilePath                ( takeBaseName )

import           Intentio.Compiler              ( Assembly
                                                , assemblyModules
                                                , mkModuleMap
                                                , Compile
                                                , CompilePure
                                                , Module(..)
                                                , impurify
                                                , pushDiagnostics
                                                )
import           Intentio.Diagnostics           ( Diagnostic(..) )

import           Language.Intentio.AST          ( ModuleSource )
import           Language.Intentio.Parser       ( ParserError
                                                , parseModule
                                                )

newtype SourceFile = SourceFile { _sourceFilePath :: FilePath }
  deriving (Show, Eq, Ord)
makeLenses ''SourceFile

instance Module SourceFile where
  type ItemTy SourceFile = Void
  _moduleName = toS . takeBaseName . _sourceFilePath
  _moduleItems = const []

parseSourceFileSet :: Assembly SourceFile -> Compile (Assembly ModuleSource)
parseSourceFileSet srcAsm = do
  mrs <-
    liftIO
    . mapConcurrently parseSourceFile_
    . toList
    $ (srcAsm ^. assemblyModules)
  mods <- unfold . foldl' doFold (Right []) $ mrs
  return $ srcAsm & assemblyModules .~ mkModuleMap (fromList mods)
 where
  doFold (Right ms) (Right m) = Right (m : ms)
  doFold (Right _ ) (Left  e) = Left [e]
  doFold (Left  es) (Left  e) = Left (e : es)
  doFold (Left  es) (Right _) = Left es

  unfold (Right l) = return l
  unfold (Left  e) = do
    pushDiagnostics . concatMap toDiag . reverse $ e
    unreachable

readSourceFile :: SourceFile -> Compile Text
readSourceFile = liftIO . readSourceFile_

parseSourceFile :: SourceFile -> Compile ModuleSource
parseSourceFile f = readSourceFile >=> impurify . parseSourceText f $ f

parseSourceText :: SourceFile -> Text -> CompilePure ModuleSource
parseSourceText f t = toCompile $ parseSourceText_ f t
 where
  toCompile (Right moduleSource) = return moduleSource
  toCompile (Left  parseErrors ) = do
    pushDiagnostics (toDiag parseErrors)
    unreachable

readSourceFile_ :: SourceFile -> IO Text
readSourceFile_ = readFile . _sourceFilePath

parseSourceFile_ :: SourceFile -> IO (Either ParserError ModuleSource)
parseSourceFile_ f = parseSourceText_ f <$> readSourceFile_ f

parseSourceText_ :: SourceFile -> Text -> Either ParserError ModuleSource
parseSourceText_ f = parseModule (_moduleName f) (_sourceFilePath f)

toDiag :: ParserError -> [Diagnostic]
toDiag = undefined
