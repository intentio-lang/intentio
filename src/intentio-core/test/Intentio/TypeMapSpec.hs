module Intentio.TypeMapSpec where

import           Intentio.Prelude        hiding ( empty
                                                , null
                                                , at
                                                )

import           Test.Hspec

import           Intentio.TypeMap

spec :: Spec
spec = parallel $ do
  describe "constructing and sizes" $ do
    it "empty makes empty type map" $ do
      null empty `shouldBe` True
      size empty `shouldBe` 0

    it "singleton make type map with one item" $ do
      null (singleton ()) `shouldBe` False
      size (singleton ()) `shouldBe` 1

  describe "lookup" $ do
    it "returns (Just x) if x is in the map" $ do
      lookup @() (singleton ()) `shouldBe` Just ()

    it "returns Nothing if x is not in the map" $ do
      lookup @() empty `shouldBe` Nothing

  describe "findWithDefault" $ do
    it "returns x if x is in the map" $ do
      findWithDefault 'a' (singleton 'b') `shouldBe` 'b'

    it "returns default if x is not in the map" $ do
      findWithDefault 'a' empty `shouldBe` 'a'

  describe "insert" $ do
    it "inserts value if map is empty" $ do
      let m1 = empty
      lookup @() m1 `shouldBe` Nothing
      let m2 = insert () m1
      lookup @() m2 `shouldBe` Just ()

    it "inserts value if none of such type exists" $ do
      let m = singleton () & insert 'a'
      lookup @() m `shouldBe` Just ()
      lookup @Char m `shouldBe` Just 'a'

    it "replaces value if one of such type alreads" $ do
      let m1 = singleton 'a'
      lookup @Char m1 `shouldBe` Just 'a'
      let m2 = insert 'b' m1
      lookup @Char m2 `shouldBe` Just 'b'

  describe "insertWith" $ do
    it "inserts value if map is empty without calling f" $ do
      let m = empty & insertWith @Text (<>) "a"
      lookup @Text m `shouldBe` Just "a"

    it "inserts value if none of such type exists without calling f" $ do
      let m = singleton () & insertWith @Text (<>) "a"
      lookup @() m `shouldBe` Just ()
      lookup @Text m `shouldBe` Just "a"

    it "replaces value if one of such type alread exists also calling f" $ do
      let m = singleton @Text "b" & insertWith @Text (<>) "a"
      lookup @Text m `shouldBe` Just "ab"

  describe "delete" $ do
    it "does nothing to empty map" $ do
      size (delete @() empty) `shouldBe` 0

    it "does nothing to map without specified type" $ do
      size (delete @() $ singleton 'a') `shouldBe` 1

    it "deletes value of specified type from map" $ do
      size (delete @() $ singleton ()) `shouldBe` 0

  describe "adjust" $ do
    it "changes value of specified type from map" $ do
      let m = singleton @Int 1 & adjust @Int (+ 1)
      lookup @Int m `shouldBe` Just 2

  describe "update" $ do
    it "changes value of specified type from map if f returned Just" $ do
      let m = singleton @Int 1 & update @Int (Just . (+ 1))
      lookup @Int m `shouldBe` Just 2

    it "deletes value of specified type from map if f returned Nothing" $ do
      let m = singleton @Int 1 & update @Int (const Nothing)
      lookup @Int m `shouldBe` Nothing

  describe "alter" $ do
    it "adds value of specified type to map" $ do
      let f = \case
            Nothing -> Just 1
            Just _  -> error "expected Nothing"
      let m = empty & alter @Int f
      lookup @Int m `shouldBe` Just 1

    it "changes value of specified type from map" $ do
      let f = \case
            (Just x) -> Just $ x + 1
            Nothing  -> error "expected Just"
      let m = singleton @Int 1 & alter @Int f
      lookup @Int m `shouldBe` Just 2

    it "deletes value of specified type from map" $ do
      let m = singleton @Int 1 & alter @Int (const Nothing)
      lookup @Int m `shouldBe` Nothing

  describe "at" $ do
    it "gets Nothing if it does not exist" $ do
      empty ^. at @() `shouldBe` Nothing

    it "gets value if it exists" $ do
      singleton () ^. at @() `shouldBe` Just ()
