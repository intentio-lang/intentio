module Intentio.Codegen.SymbolNames.ManglingSpec where

import           Intentio.Prelude

import           Data.Char                      ( isAsciiLower
                                                , isAsciiUpper
                                                , isDigit
                                                )
import qualified Data.Text                     as T
import           Test.Hspec
import           Test.QuickCheck

import           Intentio.Codegen.SymbolNames.Mangling

isGasSymbolChar :: Char -> Bool
isGasSymbolChar c | isAsciiLower c = True
                  | isAsciiUpper c = True
                  | isDigit c      = True
                  | c == '_'       = True
                  | otherwise      = False

spec :: Spec
spec = parallel $ do
  describe "mangle" $ do
    it "should always return names matching [a-zA-Z0-9_]" . property $ \s ->
      T.all isGasSymbolChar . mangle . fmap toS $ (s :: [String])
