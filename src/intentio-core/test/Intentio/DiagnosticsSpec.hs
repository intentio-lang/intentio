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

      it "should not include file name if empty" $ do
        diagnosticShow (sourcePos ()) `shouldBe` "1:1"

      it "should increment numbers by one" $ do
        diagnosticShow (SourcePos "" 4 0) `shouldBe` "5:1"
