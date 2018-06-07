module Language.Intentio.LexerSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.TestUtil.Fixture      ( runFileFixtures )

import           Language.Intentio.Lexer        ( lex )

spec :: Spec
spec = describe "lexer" $ do
  runFileFixtures "lexer" (lex "<test>")
