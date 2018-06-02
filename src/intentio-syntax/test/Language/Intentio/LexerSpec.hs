module Language.Intentio.LexerSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Language.Intentio.Debug        ( SyntaxDebugPrint
                                                , syntaxDebugPrint
                                                )
import           Language.Intentio.Lexer        ( lex )

import           Intentio.TestUtil.Fixture      ( FixtureMaterializable
                                                , runFileFixtures
                                                , fixtureMaterialize
                                                )

instance (Show l, SyntaxDebugPrint r) => FixtureMaterializable (Either l r)
 where
  fixtureMaterialize (Left l) = "[ERROR]\n" <> show l
  fixtureMaterialize (Right r) = syntaxDebugPrint r

spec :: Spec
spec = describe "lexer" $ do
  runFileFixtures "lexer" (lex "<test>")
