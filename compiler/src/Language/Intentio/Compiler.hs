module Language.Intentio.Compiler
  ( SourceFile(..)
  , sourceFilePath
  , SourceText(..)
  , sourceTextModuleName
  , sourceTextFilePath
  , sourceTextContent
  , parseSourceFiles
  , parseSourceTexts
  , readSourceFile
  , parseSourceFile
  , parseSourceText
  )
where

import           Intentio.Prelude

import qualified Data.List.NonEmpty            as NE
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Data.Text.IO                   ( readFile )
import           System.FilePath                ( takeBaseName )
import qualified Text.Megaparsec.Error         as MPE

import           Intentio.Compiler              ( Assembly
                                                , assemblyModules
                                                , mkModuleMap
                                                , Compile
                                                , CompilePure
                                                , CompileT
                                                , liftIOE
                                                , Module(..)
                                                , ModuleName(..)
                                                , moduleName
                                                , impurify
                                                , pushDiagnostics
                                                )
import           Intentio.Diagnostics           ( Diagnostic
                                                , SourcePos(..)
                                                , HasSourcePos(..)
                                                , sourcePos
                                                , cerror
                                                , cnote
                                                )

import           Language.Intentio.AST          ( ModuleSource )
import           Language.Intentio.Parser       ( ParserError
                                                , parseModule
                                                )

newtype SourceFile = SourceFile { _sourceFilePath :: FilePath }
  deriving (Show, Eq, Ord)

data SourceText = SourceText
  { _sourceTextModuleName :: ModuleName
  , _sourceTextFilePath :: FilePath
  , _sourceTextContent :: Text
  }
  deriving (Show, Eq)

makeLenses ''SourceFile
makeLenses ''SourceText

instance HasSourcePos SourceFile where
  _sourcePos SourceFile { _sourceFilePath = p } = SourcePos p 0 0

instance Module SourceFile where
  type ItemTy SourceFile = Void
  _moduleName  = filePathToModName . _sourceFilePath
  _moduleItems = const []

instance HasSourcePos SourceText where
  _sourcePos SourceText { _sourceTextFilePath = p } = SourcePos p 0 0

instance Module SourceText where
  type ItemTy SourceText = Void
  _moduleName  = _sourceTextModuleName
  _moduleItems = const []

parseSourceFiles :: Assembly SourceFile -> Compile (Assembly ModuleSource)
parseSourceFiles srcAsm = do
  let srcMl = srcAsm ^. assemblyModules & toList
  dstRs <- liftIOE $ mapM parseSourceFile_ srcMl
  dstMl <- parseResultsToCompile dstRs
  let dstMm = mkModuleMap $ NE.fromList dstMl
  return $ srcAsm & assemblyModules .~ dstMm

parseSourceTexts :: Assembly SourceText -> CompilePure (Assembly ModuleSource)
parseSourceTexts srcAsm = do
  let srcMl = srcAsm ^. assemblyModules & toList
  let dstRs = fmap parseSourceText_ srcMl
  dstMl <- parseResultsToCompile dstRs
  let dstMm = mkModuleMap $ NE.fromList dstMl
  return $ srcAsm & assemblyModules .~ dstMm

readSourceFile :: SourceFile -> Compile SourceText
readSourceFile = liftIO . readSourceFile_

parseSourceFile :: SourceFile -> Compile ModuleSource
parseSourceFile = readSourceFile >=> impurify . parseSourceText

parseSourceText :: SourceText -> CompilePure ModuleSource
parseSourceText = toCompile . parseSourceText_
 where
  toCompile (Right moduleSource) = return moduleSource
  toCompile (Left  parseErrors ) = do
    pushDiagnostics (toDiag parseErrors)
    unreachable

readSourceFile_ :: SourceFile -> IO SourceText
readSourceFile_ f = do
  let _sourceTextFilePath   = f ^. sourceFilePath
  let _sourceTextModuleName = filePathToModName _sourceTextFilePath
  _sourceTextContent <- readFile _sourceTextFilePath
  return $ SourceText { .. }

parseSourceFile_ :: SourceFile -> IO (Either ParserError ModuleSource)
parseSourceFile_ = fmap parseSourceText_ . readSourceFile_

parseSourceText_ :: SourceText -> Either ParserError ModuleSource
parseSourceText_ f = parseModule (f ^. moduleName)
                                 (f ^. sourceTextFilePath)
                                 (f ^. sourceTextContent)

filePathToModName :: FilePath -> ModuleName
filePathToModName = ModuleName . toS . takeBaseName

toDiag :: ParserError -> Seq Diagnostic
toDiag parserError = headDiag <| S.fromList stackDiags
 where
  hsp :| stp = MPE.errorPos parserError
  stackDiags = fmap stackDiag stp
  headDiag   = cerror (hsp ^. sourcePos) headMsg
  headMsg    = T.strip . toS . MPE.parseErrorTextPretty $ parserError
  stackDiag p = cnote (p ^. sourcePos) "included from here"

parseResultsToCompile :: Monad m => [Either ParserError a] -> CompileT m [a]
parseResultsToCompile = unFold . foldr' bucket (Right [])
 where
  bucket (Right m) (Right ms) = Right (m : ms)
  bucket (Left  e) (Right _ ) = Left [e]
  bucket (Left  e) (Left  es) = Left (e : es)
  bucket (Right _) (Left  es) = Left es

  unFold (Right l) = return l
  unFold (Left  e) = do
    pushDiagnostics . mconcat . fmap toDiag $ e
    unreachable
