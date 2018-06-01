module Language.Intentio.Debug where

import           Intentio.Prelude

import qualified Data.Text                     as T

class SyntaxDebugPrint a where
  syntaxDebugPrint :: a -> Text

instance SyntaxDebugPrint a => SyntaxDebugPrint [a] where
  syntaxDebugPrint = T.unlines . map syntaxDebugPrint
