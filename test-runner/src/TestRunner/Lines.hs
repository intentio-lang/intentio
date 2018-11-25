module TestRunner.Lines where

import           Intentio.Prelude

import qualified Data.Text                     as T

newtype Lines = Lines {unlines :: Text}

instance Semigroup Lines where
  Lines a <> Lines b = Lines $ a <> "\n" <> b

instance Monoid Lines where
  mempty  = Lines ""
  mconcat = Lines . T.unlines . fmap unlines

tellLine :: (MonadWriter Lines m) => Text -> m ()
tellLine = tell . Lines
