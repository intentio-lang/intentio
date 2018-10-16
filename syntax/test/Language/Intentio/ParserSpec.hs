module Language.Intentio.ParserSpec where

import           Intentio.Prelude        hiding ( some )

import           Test.Hspec

import           Text.Megaparsec                ( eof
                                                , parse
                                                , some
                                                )

import           Intentio.TestUtil.Fixture      ( runFileFixtures )

import           Language.Intentio.Assembly     ( ModuleName(..) )
import           Language.Intentio.Parser       ( parseModule
                                                , itemDecl
                                                , exportDecl
                                                , importDecl
                                                , stmt
                                                , Parser
                                                )
import           Language.Intentio.AST          ( ExportDecl
                                                , ImportDecl
                                                )

data ExportImportDecl
  = ExportDecl ExportDecl | ImportDecl ImportDecl
  deriving (Show, Eq, Generic)

instance ToJSON ExportImportDecl
instance FromJSON ExportImportDecl

exportImportDecl :: Parser ExportImportDecl
exportImportDecl = 
  (ExportDecl <$> exportDecl) 
  <|> (ImportDecl <$> importDecl)

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    runFileFixtures "parser-module" (parseModule (ModuleName "test") "test.ieo")
    runFileFixtures "parser-itemdecl" (parse (some itemDecl <* eof) "test.ieo")
    runFileFixtures "parser-expr" (parse (stmt <* eof) "test.ieo")
    runFileFixtures "parser-importexport" (parse (some exportImportDecl <* eof) "test.ieo")
