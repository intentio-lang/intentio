module Intentio.Compiler.MonadSpec where

import           Intentio.Prelude

import           System.IO.Error                ( userError )
import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Diagnostics

spec :: Spec
spec = parallel $ do
  describe "liftIOE" $ do
    it "lifting IO computation works correctly" $ do
      let comp = liftIOE . return $ ()
      res <- compileFresh comp
      res `shouldBe` Right ()

    it "lifting IO computation converts IO errors to ICE diagnostics" $ do
      let comp = void (liftIOE . ioError $ userError "test")
      (res, ctx) <- runCompileFresh comp
      res `shouldSatisfy` has _Nothing
      let diag = ctx ^. compileDiagnostics ^?! _head
      (diag ^. diagnosticSeverity) `shouldBe` DiagnosticICE

  describe "pushDiagnostic" $ do
    it "warnings do not stop compilation" $ do
      let flow = do
            pushDiagnostic $ cwarningFor () "1"
            pushDiagnostic $ cwarning (SourcePos "" 3 4) "2"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Just
      (ctx ^. compileDiagnostics & length) `shouldBe` 2

    it "errors do stop compilation" $ do
      let flow = do
            pushDiagnostic $ cerrorFor () "Error"
            pushDiagnostic $ cerrorFor () "This should not be emitted"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnostics & length) `shouldBe` 1

    it "ICEs do stop compilation" $ do
      let flow       = pushDiagnostic $ iceFor () "BAAM!"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnostics & length) `shouldBe` 1

    it "order of diagnostics is preserved for compile* methods" $ do
      let flow = do
            pushDiagnostic $ cwarningFor () "1"
            pushDiagnostic $ cwarning (SourcePos "" 3 4) "2"
            pushDiagnostic $ cerrorFor () "Error"
            pushDiagnostic $ cerrorFor () "This should not be emitted"
      let diags = compilePureFresh flow ^?! _Left
      (^. diagnosticMessage) <$> diags `shouldBe` ["1", "2", "Error"]

  describe "pushDiagnosticE" $ do
    it "warnings do not stop compilation" $ do
      let flow = do
            pushDiagnosticE $ cwarningFor () "1"
            pushDiagnosticE $ cwarning (SourcePos "" 3 4) "2"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Just
      (ctx ^. compileDiagnostics & length) `shouldBe` 2

    it "errors do not stop compilation" $ do
      let flow = do
            pushDiagnosticE $ cerrorFor () "Error"
            pushDiagnosticE $ cerrorFor () "This should be emitted"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Just
      (ctx ^. compileDiagnostics & length) `shouldBe` 2

    it "ICEs do stop compilation" $ do
      let flow = do
            pushDiagnosticE $ iceFor () "BAAM!"
            pushDiagnosticE $ cerrorFor () "This should be emitted"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnostics & length) `shouldBe` 1

    it "order of diagnostics is preserved for compile* methods" $ do
      let flow = do
            pushDiagnosticE $ cwarningFor () "1"
            pushDiagnosticE $ cwarning (SourcePos "" 3 4) "2"
            pushDiagnosticE $ cerrorFor () "Error"
            pushDiagnosticE $ iceFor () "ICE"
            pushDiagnosticE $ cerrorFor () "This should be emitted"
      let diags = compilePureFresh flow ^?! _Left
      (^. diagnosticMessage) <$> diags `shouldBe` ["1", "2", "Error", "ICE"]

  describe "pushDiagnostics" $ do
    it "warnings do not stop compilation" $ do
      let flow = do
            pushDiagnostics
              [cwarningFor () "1", cwarning (SourcePos "" 3 4) "2"]
            pushDiagnostic $ cwarningFor () "1"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Just
      (ctx ^. compileDiagnostics & length) `shouldBe` 3

    it "errors do stop compilation" $ do
      let flow = do
            pushDiagnostics
              [cerrorFor () "Error", cerrorFor () "This should be emitted"]
            pushDiagnostic $ cwarningFor () "1"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnostics & length) `shouldBe` 2

    it "ICEs do stop compilation" $ do
      let flow = do
            pushDiagnostics [iceFor () "BAAM!", iceFor () "Other BAAM!"]
            pushDiagnostic $ cwarningFor () "1"
      let (res, ctx) = runCompilePureFresh flow
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnostics & length) `shouldBe` 2

    it "order of diagnostics is preserved for compile* methods" $ do
      let flow = do
            pushDiagnostics
              [cwarningFor () "1", cwarning (SourcePos "" 3 4) "2"]
            pushDiagnostics [cerrorFor () "E1", cerrorFor () "E2"]
            pushDiagnostics [cerrorFor () "This should not be emitted"]
      let diags = compilePureFresh flow ^?! _Left
      (^. diagnosticMessage) <$> diags `shouldBe` ["1", "2", "E1", "E2"]
