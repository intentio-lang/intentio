module Language.Intentio.Debug where

import           Intentio.Prelude

import qualified Data.Text                     as T

class SyntaxDebugPrint a where
  syntaxDebugPrint :: a -> Text

instance (SyntaxDebugPrint a, Foldable f, Functor f) => SyntaxDebugPrint (f a)
 where
  syntaxDebugPrint = T.unlines . toList . map syntaxDebugPrint
