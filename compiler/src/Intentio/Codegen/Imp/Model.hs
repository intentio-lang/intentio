module Intentio.Codegen.Imp.Model
  ( module Intentio.Codegen.Imp.Model
  , module X
  , getAnn
  , setAnn
  , ann
  )
where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM

import           Intentio.Annotated             ( Annotated(..) )
import           Intentio.Compiler             as X
                                                ( ModuleName(..)
                                                , ItemName(..)
                                                )
import           Intentio.Hir                  as X
                                                ( ItemId(..)
                                                , unItemId
                                                , BodyId(..)
                                                , unBodyId
                                                , VarId(..)
                                                , unVarId
                                                , Param(..)
                                                , paramVarId
                                                , Var(..)
                                                , varAnn
                                                , varId
                                                , varName
                                                , Lit(..)
                                                , litAnn
                                                , litSourcePos
                                                , litKind
                                                , LitKind(..)
                                                , _IntegerLit
                                                , _FloatLit
                                                , _StringLit
                                                , _NoneLit
                                                , Path(..)
                                                , pathAnn
                                                , pathKind
                                                , PathKind(..)
                                                , _ToVar
                                                , _ToItem
                                                )

data Body a = Body
  { _bodyAnn    :: a
  , _bodyParams :: [Param a]
  , _bodyVars   :: IM.IntMap (Var a)
  , _bodyVarIds :: [VarId]
  , _bodyBlock  :: Block a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Body a)
instance FromJSON a => FromJSON (Body a)

data Block a = Block
  { _blockAnn   :: a
  , _blockStmts :: [Stmt a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Block a)
instance FromJSON a => FromJSON (Block a)

data Stmt a = Stmt
  { _stmtAnn  :: a
  , _stmtKind :: StmtKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Stmt a)
instance FromJSON a => FromJSON (Stmt a)

data StmtKind a
  = AssignStmt VarId (Expr a)
  | WhileStmt VarId (Block a)
  | IfStmt VarId (Block a)
  | ReturnStmt VarId
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StmtKind a)
instance FromJSON a => FromJSON (StmtKind a)

data Expr a = Expr
  { _exprAnn  :: a
  , _exprKind :: ExprKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Expr a)
instance FromJSON a => FromJSON (Expr a)

data ExprKind a
  = PathExpr (Path a)
  | LitExpr (Lit a)
  | UnExpr UnOpKind VarId
  | BinExpr BinOpKind VarId
  | CallGlobalExpr ItemId [VarId]
  | CallLocalExpr VarId [VarId]
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ExprKind a)
instance FromJSON a => FromJSON (ExprKind a)

data UnOpKind = UnNeg
  deriving (Show, Eq, Generic)

instance ToJSON UnOpKind
instance FromJSON UnOpKind

data BinOpKind
  = BinAdd
  | BinDiv
  | BinEq
  | BinGt
  | BinGtEq
  | BinLt
  | BinLtEq
  | BinMul
  | BinNeq
  | BinSEq
  | BinSNeq
  | BinSub
  | BinXor
  deriving (Show, Eq, Generic)

instance ToJSON BinOpKind
instance FromJSON BinOpKind

makeLenses ''Body
makeLenses ''Stmt
makePrisms ''StmtKind
makeLenses ''Expr
makePrisms ''ExprKind
makeLenses ''Block
makePrisms ''UnOpKind
makePrisms ''BinOpKind

instance Annotated Body where
  ann = bodyAnn

instance Annotated Block where
  ann = blockAnn

instance Annotated Stmt where
  ann = stmtAnn

instance Annotated Expr where
  ann = exprAnn

bodyVar :: VarId -> Traversal' (Body a) (Var a)
bodyVar (VarId i) = bodyVars . ix i

findParamVar :: Body a -> Param a -> Maybe (Var a)
findParamVar body param = body ^? (bodyVars . ix i)
  where i = param ^. paramVarId . unVarId
