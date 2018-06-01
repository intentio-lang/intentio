module Language.Intentio.LexerSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Language.Intentio.Lexer        ( lex )

import           Intentio.TestUtil.Fixture      ( runFileFixtures )

spec :: Spec
spec = describe "lexer" $ do
  context "fixture:" $ runFileFixtures "lexer" (show . lex "<test>")
