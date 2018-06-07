module Language.Intentio.ParserSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.TestUtil.Fixture      ( runFileFixtures )

import           Language.Intentio.Parser       ( parseModule
                                                , parseItemDecl
                                                , parseExpr
                                                )

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    runFileFixtures "parser-module"   (parseModule "<test>")
    runFileFixtures "parser-itemdecl" (parseItemDecl "<test>")
    runFileFixtures "parser-expr"     (parseExpr "<test>")
