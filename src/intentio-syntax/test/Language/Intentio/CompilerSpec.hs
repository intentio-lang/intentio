module Language.Intentio.CompilerSpec where

import           Intentio.Prelude

import           Data.List.NonEmpty             ( fromList )
import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Diagnostics

import           Language.Intentio.Compiler

goodfileText :: Text
goodfileText = "fun f(a) {}"

goodfile :: SourceText
goodfile = SourceText (ModuleName "good") "good.ieo" goodfileText

badfileText :: Text
badfileText = "fun f"

badfile :: SourceText
badfile = SourceText (ModuleName "bad") "bad.ieo" badfileText

spec :: Spec
spec = parallel $ do
  let aname = AssemblyName "test"

  describe "given good assembly" $ do
    let asm = mkAssembly Library aname "out" $ fromList [goodfile]

    it "should parse it" $ do
      compilePureFresh (parseSourceTexts asm) `shouldSatisfy` has _Right

  describe "given bad assembly" $ do
    let asm = mkAssembly Library aname "out" $ fromList [goodfile, badfile]

    it "should return errors" $ do
      compilePureFresh (parseSourceTexts asm) `shouldBe` Left
        [ cerror
            (SourcePos "bad.ieo" 0 5)
            "unexpected end of input\nexpecting '(' or the rest of identifier"
        ]
