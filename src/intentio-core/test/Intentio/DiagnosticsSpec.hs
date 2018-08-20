module Intentio.DiagnosticsSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Diagnostics

spec :: Spec
spec = parallel $ do
  describe "SourcePos" $ do
    describe "diagnosticPrint" $ do
      it "should show position info separated by :" $ do
        diagnosticShow (SourcePos "test" 0 0) `shouldBe` "test:1:1"

      it "should show ? for nullary positon" $ do
        diagnosticShow (_sourcePos ()) `shouldBe` "?"

      it "should not include file name if empty" $ do
        diagnosticShow (SourcePos "" 4 0) `shouldBe` "5:1"

      it "should increment numbers by one" $ do
        diagnosticShow (SourcePos "file" 4 0) `shouldBe` "file:5:1"
