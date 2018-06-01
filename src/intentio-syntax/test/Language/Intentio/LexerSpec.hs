module Language.Intentio.LexerSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Language.Intentio.Lexer

import           Intentio.TestUtil

spec :: Spec
spec = describe "lexer" $ do
  it "test" $ do
    getFixtures "lexer" >>= print
    1 `shouldBe` 1
