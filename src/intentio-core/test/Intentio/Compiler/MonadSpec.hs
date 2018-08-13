module Intentio.Compiler.MonadSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Compiler.DummyAssembly
import           Intentio.Diagnostics

type FrobnicatedAssembly = Assembly FrobnicatedModule

newtype FrobnicatedModule = FrobnicatedModule DummyModule
  deriving (Show, Eq)
makeWrapped ''FrobnicatedModule

instance Module FrobnicatedModule where
  type ItemTy FrobnicatedModule = FrobnicatedItem
  _moduleName = view (_Wrapped . moduleName)
  _moduleItems = map FrobnicatedItem . view (_Wrapped . moduleItems)

newtype FrobnicatedItem = FrobnicatedItem DummyItem
  deriving (Show, Eq)
makeWrapped ''FrobnicatedItem

instance Item FrobnicatedItem where
  _itemName = view (_Wrapped . itemName)

frobnicate :: DummyAssembly -> CompilePure FrobnicatedAssembly
frobnicate asm = return $ asm & assemblyModules %~ fmap FrobnicatedModule

addSmiles :: FrobnicatedAssembly -> CompilePure FrobnicatedAssembly
addSmiles a = return $ a & assemblyName %~ (<> ":)") & assemblyModules %~ fmap
  (\m ->
    m
      &  _Wrapped
      .  dummyModuleName
      %~ (<> ":)")
      &  _Wrapped
      .  dummyModuleItems
      %~ fmap (\i -> i & dummyItemName %~ (<> ":)"))
  )

ioStep :: a -> Compile a
ioStep = return

spec :: Spec
spec = parallel $ do
  describe "frobnicating dummy assembly" $ do
    it "just frobnicate" $ do
      let flow = frobnicate
      let asm = compilePureFresh (flow dummyAssembly1) ^?! _Right
      asm ^. assemblyName `shouldBe` "dummy_assembly"

    it "frobnicate and add smiles" $ do
      let flow = frobnicate >=> addSmiles
      let asm  = compilePureFresh (flow dummyAssembly1) ^?! _Right
      asm ^. assemblyName `shouldBe` "dummy_assembly:)"

    it "frobnicate and add smiles in IO" $ do
      let flow = ioStep >=> impurify . frobnicate >=> impurify . addSmiles
      asm <- compileFresh (flow dummyAssembly1)
      asm ^?! _Right ^. assemblyName `shouldBe` "dummy_assembly:)"

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
