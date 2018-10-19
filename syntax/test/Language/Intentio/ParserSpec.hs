module Language.Intentio.ParserSpec where

import           Intentio.Prelude        hiding ( some )

import           Test.Hspec

import           Intentio.TestUtil.Fixture      ( runFileFixtures )

import           Language.Intentio.Assembly     ( ModuleName(..) )
import           Language.Intentio.Parser       ( parseModule
                                                , parseItemDecls
                                                , parseStmts
                                                , lex
                                                )

spec :: Spec
spec = parallel $ do
  describe "lexer" $ do
    runFileFixtures "lexer" (lex "test.ieo")

  describe "parser" $ do
    runFileFixtures "parser-module" (parseModule (ModuleName "test") "test.ieo")
    runFileFixtures "parser-itemdecl" (parseItemDecls "test.ieo")
    runFileFixtures "parser-stmt" (parseStmts "test.ieo")
