module Intentio.Util.NodeIdSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Diagnostics
import           Intentio.Hir
import qualified Intentio.Util.NodeId          as NodeId

-- | Handwritten module HIR equivalent to following code
--   (except source positions):
-- @
--     export (id)
--     import some:_unused
--     fun id(x) {
--       return x;
--     }
-- @
someHir :: Module ()
someHir = Module ()
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
        (BodyId 0)
        [Param (VarId 0)]
        (fromList [(0, Var () (SourcePos "id.ieo" 10 10) (VarId 0) "x")])
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

someHirWithId :: Module NodeId.NodeId
someHirWithId = Module (NodeId.mk 0)
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
      , Item (NodeId.mk 1)
             (SourcePos "id.ieo" 10 0)
             (ItemId 0)
             (Just $ ItemName "id")
             (FnItem (BodyId 0))
      )
    ]
  bodies = fromList
    [ ( 0
      , Body
        (NodeId.mk 2)
        (BodyId 0)
        [Param (VarId 0)]
        (fromList
          [(0, Var (NodeId.mk 3) (SourcePos "id.ieo" 10 10) (VarId 0) "x")]
        )
        [VarId 0]
        (Expr
          (NodeId.mk 4)
          (SourcePos "id.ieo" 10 0)
          (ReturnExpr
            (Expr
              (NodeId.mk 5)
              (SourcePos "id.ieo" 11 2)
              (PathExpr
                (Path (NodeId.mk 6) (SourcePos "id.ieo" 11 10) (ToVar (VarId 0))
                )
              )
            )
          )
        )
      )
    ]

spec :: Spec
spec = parallel $ do
  describe "assign" $ do
    it "should sequentially assign node ids to Annotated nodes" $ do
      NodeId.assign someHir `shouldBe` someHirWithId
