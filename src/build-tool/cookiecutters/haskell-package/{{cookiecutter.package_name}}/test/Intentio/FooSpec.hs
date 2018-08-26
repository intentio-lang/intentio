module Intentio.FooSpec where

import           Intentio.Prelude

import           Test.Hspec

spec = do
  describe "{{cookiecutter.package_name}}" $ do
    it "should foobar" $ do
      1 `shouldBe` 1
