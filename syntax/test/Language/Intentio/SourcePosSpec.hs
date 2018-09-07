module Language.Intentio.SourcePosSpec where

import           Intentio.Prelude

import           Data.Aeson                     ( encode
                                                , decode
                                                )
import           Test.Hspec
import           Test.QuickCheck

import           Language.Intentio.SourcePos

spec :: Spec
spec = parallel $ do
  it "should be JSON-serializable" . property $ \f l c ->
    let p = SourcePos f l c in (decode . encode) p == Just p
