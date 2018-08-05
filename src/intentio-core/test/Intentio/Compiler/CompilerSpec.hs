module Intentio.Compiler.CompilerSpec where

import           Intentio.Prelude        hiding ( moduleName )

import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Compiler.DummyAssembly

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
addSmiles a = return $ a
                     & assemblyName %~ (<> ":)")
                     & assemblyModules %~ fmap (\m -> m
                        & _Wrapped . dummyModuleName %~ (<> ":)")
                        & _Wrapped . dummyModuleItems %~ fmap (\i -> i
                          & dummyItemName %~ (<> ":)")
                        )
                     )

spec :: Spec
spec = parallel $ do
  describe "frobnicating dummy assembly" $ do
    let runFlow flow = compilePureFresh (flow dummyAssembly1)
    let runFlowL f = runFlow f ^?! _Left
    let runFlowR f = runFlow f ^?! _Right

    it "just frobnicate" $ do
      let asm = runFlowR frobnicate
      asm ^. assemblyName `shouldBe` "dummy_assembly"

    it "frobnicate and add smiles" $ do
      let asm = runFlowR $ frobnicate >=> addSmiles
      asm ^. assemblyName `shouldBe` "dummy_assembly:)"
