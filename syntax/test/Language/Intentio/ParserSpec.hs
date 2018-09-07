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
                                                , expr
                                                )

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    runFileFixtures "parser-module" (parseModule (ModuleName "test") "test.ieo")
    runFileFixtures "parser-itemdecl" (parse (some itemDecl <* eof) "test.ieo")
    runFileFixtures "parser-expr" (parse (expr <* eof) "test.ieo")
