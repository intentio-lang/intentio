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

warnings :: a -> CompilePure a
warnings a = do
  pushDiagnostic $ cwarningFor () "1"
  pushDiagnostic $ cwarning (SourcePos "" 3 4) "2"
  return a

anError :: a -> CompilePure a
anError a = do
  pushDiagnostic $ cerrorFor () "Error"
  pushDiagnostic $ cerrorFor () "This should not be emitted"
  return a

anICE :: a -> CompilePure a
anICE a = do
  pushDiagnostic $ iceFor () "BAAM!"
  return a

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

  describe "diagnostics" $ do
    it "warnings do not stop compilation" $ do
      let flow       = warnings
      let (res, ctx) = runCompilePureFresh (flow ())
      res `shouldSatisfy` has _Just
      (ctx ^. compileDiagnosticsStack & length) `shouldBe` 2

    it "errors do not stop compilation" $ do
      let flow       = anError
      let (res, ctx) = runCompilePureFresh (flow ())
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnosticsStack & length) `shouldBe` 1

    it "ICEs do not stop compilation + IO" $ do
      let flow = impurify . anICE >=> ioStep
      (res, ctx) <- runCompileFresh (flow ())
      res `shouldSatisfy` has _Nothing
      (ctx ^. compileDiagnosticsStack & length) `shouldBe` 1

    it "order of diagnostics is preserved for compile* methods" $ do
      let flow  = warnings >=> anError
      let diags = compilePureFresh (flow ()) ^?! _Left
      (^. diagnosticMessage) <$> diags `shouldBe` ["1", "2", "Error"]
