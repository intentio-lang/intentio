module Intentio.Compiler.AssemblySpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Compiler
import           Intentio.Compiler.DummyAssembly

spec :: Spec
spec = parallel $ do
  describe "librarySafe" $ do
    it "should build an library assembly" $ do
      let libM = librarySafe "test" (dummyModule :| [])
      libM `shouldSatisfy` has _Right
      let lib = libM ^?! _Right
      lib ^. assemblyName `shouldBe` "test"

  describe "programSafe" $ do
    it "should build an program assembly given existing main module" $ do
      let mainName = dummyModule ^. moduleName
      let progM    = programSafe "test" mainName (dummyModule :| [])
      progM `shouldSatisfy` has _Right
      let prog = progM ^?! _Right
      prog ^. assemblyName `shouldBe` "test"
      prog ^. assemblyMainModuleName `shouldBe` Just mainName

    it "should error if main module is not defined" $ do
      let progM = programSafe "test" "totally_not_main" (dummyModule :| [])
      progM `shouldBe` Left MainDoesNotExist
