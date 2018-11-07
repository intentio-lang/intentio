module Intentio.HirSpec where

import           Intentio.Prelude

import           Data.Aeson                     ( decode
                                                , encode
                                                )
import           Test.Hspec

import           Intentio.Diagnostics
import           Intentio.Hir

-- | Handwritten module HIR equivalent to following code
--   (except source positions):
-- @
--     export (id)
--     import some:_unused
--     fun id(x) {
--       return x;
--     }
-- @
idModule :: Module ()
idModule = Module ()
                  (SourcePos "id.ieo" 0 0)
                  (ModuleName "id")
                  (fromList [ItemName "id"])
                  (fromList [(ModuleName "some", ItemName "_unused")])
                  items
                  [ItemId 0]
                  (fromList [(ItemName "id", ItemId 0)])
                  bodies
                  [BodyId 0]
 where
  items = fromList
    [ ( 0
      , Item ()
             (SourcePos "id.ieo" 10 0)
             (ItemId 0)
             (Just $ ItemName "id")
             (FnItem (BodyId 0))
      )
    ]
  bodies = fromList
    [ ( 0
      , Body
        ()
        [Param (VarId 0)]
        (fromList
          [(0, Var () (VarId 0) (Ident () (SourcePos "id.ieo" 10 10) "x"))]
        )
        [VarId 0]
        (Expr
          ()
          (SourcePos "id.ieo" 10 0)
          (ReturnExpr
            (Expr
              ()
              (SourcePos "id.ieo" 11 2)
              (PathExpr (Path () (SourcePos "id.ieo" 11 10) (ToVar (VarId 0))))
            )
          )
        )
      )
    ]

spec :: Spec
spec = parallel $ do
  describe "HIR data structure" $ do
    it "should be JSON serializable" $ do
      (decode . encode) idModule `shouldBe` Just idModule
