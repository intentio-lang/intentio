module Language.Intentio.CompileSpec where

import           Intentio.Prelude

import           Data.List.NonEmpty             ( fromList )
import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Diagnostics

import           Language.Intentio.Compile

goodfileText :: Text
goodfileText = "fun f(a) {}"

goodfile :: SourceText
goodfile = SourceText "good" "good.ieo" goodfileText

badfileText :: Text
badfileText = "fun f"

badfile :: SourceText
badfile = SourceText "bad" "bad.ieo" badfileText

spec :: Spec
spec = parallel $ do
  describe "given good assembly" $ do
    let asm = library "test" $ fromList [goodfile]

    it "should parse it" $ do
      compilePureFresh (parseSourceTexts asm) `shouldSatisfy` has _Right

  describe "given bad assembly" $ do
    let asm = library "test" $ fromList [goodfile, badfile]

    it "should return errors" $ do
      compilePureFresh (parseSourceTexts asm) `shouldBe` Left
        [ cerror
            (SourcePos "bad.ieo" 0 5)
            "unexpected end of input\nexpecting '(' or the rest of identifier"
        ]
