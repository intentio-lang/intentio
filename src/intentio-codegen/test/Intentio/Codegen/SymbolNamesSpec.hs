module Intentio.Codegen.SymbolNamesSpec where

import           Intentio.Prelude

import           Data.Char                      ( isAsciiLower
                                                , isAsciiUpper
                                                , isDigit
                                                )
import qualified Data.Text                     as T
import           Test.Hspec
import           Test.QuickCheck

import           Intentio.Codegen.SymbolNames

isGasSymbolChar :: Char -> Bool
isGasSymbolChar c | isAsciiLower c           = True
                  | isAsciiUpper c           = True
                  | isDigit c                = True
                  | c `elem` ['.', '_', '$'] = True
                  | otherwise                = False

spec :: Spec
spec = parallel $ do
  describe "mangle" $ do
    it "should always return names matching [a-zA-Z0-9._$]" . property $ \s ->
      T.all isGasSymbolChar . mangle . fmap toS $ (s :: [String])

  describe "sanitize" $ do
    it "should always return names matching [a-zA-Z0-9._$]" . property $ \s ->
      T.all isGasSymbolChar . sanitize . toS $ (s :: String)
