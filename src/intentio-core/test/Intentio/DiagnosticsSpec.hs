module Intentio.DiagnosticsSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Diagnostics

spec :: Spec
spec = parallel $ do
  let sampleWarning = Diagnostic Warning (SourcePos "test" 0 0) "Hello!"
  let sampleICE = Diagnostic InternalCompilerError (sourcePos ()) "Bam!"

  describe "isDiagnosticErroneous" $ do
    describe "on DiagnosticSeverity" $ do
      it "should return false for Warning" $ do
        isDiagnosticErroneous Warning `shouldBe` False

      it "should return true for compile error" $ do
        isDiagnosticErroneous CompileError `shouldBe` True

      it "should return true for ICE" $ do
        isDiagnosticErroneous InternalCompilerError `shouldBe` True

    describe "on Diagnostic" $ do
      it "should return true for ICE" $ do
        isDiagnosticErroneous sampleICE `shouldBe` True

    describe "on [Diagnostic]" $ do
      it "should return false for empty list" $ do
        isDiagnosticErroneous ([] :: [Diagnostic]) `shouldBe` False

      it "should return false for warnings only" $ do
        isDiagnosticErroneous [sampleWarning, sampleWarning] `shouldBe` False

      it "should return true for mixed warning/ICE" $ do
        isDiagnosticErroneous [sampleWarning, sampleICE] `shouldBe` True

  describe "SourcePos" $ do
    describe "diagnosticPrint" $ do
      it "should show position info separated by :" $ do
        diagnosticShow (SourcePos "test" 0 0) `shouldBe` "test:1:1"

      it "should not include file name if empty" $ do
        diagnosticShow (sourcePos ()) `shouldBe` "1:1"

      it "should increment numbers by one" $ do
        diagnosticShow (SourcePos "" 4 0) `shouldBe` "5:1"
