module Intentio.Hir.ResolverSpec where

import           Intentio.Prelude

import           Test.Hspec

import           Intentio.Diagnostics
import           Intentio.Hir
import           Intentio.Hir.Resolver

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
                 [ItemId 1]
                 items
                 [ItemId 0, ItemId 1]
                 (fromList [(ItemName "id", ItemId 1)])
                 bodies
                 [BodyId 0]
 where
  items = fromList
    [ ( 0
      , Item ()
             (SourcePos "id.ieo" 5 0)
             (ItemId 0)
             Nothing
             (ImportItem (ModuleName "some") (ItemName "_unused"))
      )
    , ( 1
      , Item ()
             (SourcePos "id.ieo" 10 0)
             (ItemId 1)
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
              (PathExpr (Path () (SourcePos "id.ieo" 11 10) (Local (VarId 0))))
            )
          )
        )
      )
    ]

someHirWithId :: Module NodeId
someHirWithId = Module (mkNodeId 0)
                       (SourcePos "id.ieo" 0 0)
                       (ModuleName "id")
                       [ItemId 1]
                       items
                       [ItemId 0, ItemId 1]
                       (fromList [(ItemName "id", ItemId 1)])
                       bodies
                       [BodyId 0]
 where
  items = fromList
    [ ( 0
      , Item (mkNodeId 1)
             (SourcePos "id.ieo" 5 0)
             (ItemId 0)
             Nothing
             (ImportItem (ModuleName "some") (ItemName "_unused"))
      )
    , ( 1
      , Item (mkNodeId 2)
             (SourcePos "id.ieo" 10 0)
             (ItemId 1)
             (Just $ ItemName "id")
             (FnItem (BodyId 0))
      )
    ]
  bodies = fromList
    [ ( 0
      , Body
        (mkNodeId 3)
        [Param (VarId 0)]
        (fromList
          [ ( 0
            , Var (mkNodeId 4)
                  (VarId 0)
                  (Ident (mkNodeId 5) (SourcePos "id.ieo" 10 10) "x")
            )
          ]
        )
        [VarId 0]
        (Expr
          (mkNodeId 6)
          (SourcePos "id.ieo" 10 0)
          (ReturnExpr
            (Expr
              (mkNodeId 7)
              (SourcePos "id.ieo" 11 2)
              (PathExpr
                (Path (mkNodeId 8) (SourcePos "id.ieo" 11 10) (Local (VarId 0)))
              )
            )
          )
        )
      )
    ]

spec :: Spec
spec = parallel $ do
  describe "assignNodeIds" $ do
    it "should sequentially assign node ids to Annotated nodes" $ do
      assignNodeIds someHir `shouldBe` someHirWithId
