module Language.Intentio.LexerSpec where

import           Intentio.Prelude

import           Text.Megaparsec.Error          ( ParseError
                                                , ShowErrorComponent
                                                , ShowToken
                                                , parseErrorPretty
                                                )

import           Test.Hspec

import           Language.Intentio.Debug        ( SyntaxDebugPrint
                                                , syntaxDebugPrint
                                                )
import           Language.Intentio.Lexer        ( lex )

import           Intentio.TestUtil.Fixture      ( FixtureMaterializable
                                                , runFileFixtures
                                                , fixtureMaterialize
                                                )

instance (ShowErrorComponent e, Ord t, ShowToken t, SyntaxDebugPrint r) =>
  FixtureMaterializable (Either (ParseError t e) r)
 where
  fixtureMaterialize (Left l) = "[ERROR]\n" <> toS (parseErrorPretty l)
  fixtureMaterialize (Right r) = syntaxDebugPrint r

spec :: Spec
spec = describe "lexer" $ do
  runFileFixtures "lexer" (lex "<test>")
