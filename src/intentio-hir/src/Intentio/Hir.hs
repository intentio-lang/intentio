module Intentio.Hir where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M

import           Intentio.Compiler              ( ModuleName(..)
                                                , ItemName(..)
                                                )
import qualified Intentio.Compiler             as C
import           Intentio.Diagnostics           ( SourcePos(..)
                                                , SourcePosProvider(..)
                                                )
import           Language.Intentio.AST          ( Literal )

--------------------------------------------------------------------------------
-- HIR data structures

newtype ItemId = ItemId IM.Key
  deriving (Show, Read, Eq, Ord, Hashable, Enum, Bounded)

newtype BodyId = BodyId IM.Key
  deriving (Show, Read, Eq, Ord, Hashable, Enum, Bounded)

newtype VarId = VarId IM.Key
  deriving (Show, Read, Eq, Ord, Hashable, Enum, Bounded)

data Module = Module
  { _moduleName       :: ModuleName
  , _moduleSourcePos  :: SourcePos

  , _moduleExports    :: [ItemId]

  , _moduleItems      :: IM.IntMap Item
  , _moduleItemIds    :: [ItemId]
  , _moduleItemsNames :: M.Map ItemName ItemId

  , _moduleBodies     :: IM.IntMap Body
  , _moduleBodyIds    :: [BodyId]
  }
  deriving (Show, Eq)

instance SourcePosProvider Module where
  _sourcePos = _moduleSourcePos

instance C.Module Module where
  type ItemTy Module = Item
  _moduleName = Intentio.Hir._moduleName
  _moduleItems Module { _moduleItems = i, _moduleItemIds = n } =
    toList $ (\w -> i ^?! ix (w ^. _Wrapped)) <$> n

data Item = Item
  { _itemName       :: ItemName
  , _itemId         :: ItemId
  , _itemSourcePos  :: SourcePos
  , _itemKind       :: ItemKind
  }
  deriving (Show, Eq)

instance SourcePosProvider Item where
  _sourcePos = _itemSourcePos

instance C.Item Item where
  _itemName = Intentio.Hir._itemName

data ItemKind
  = ImportItem ModuleName ItemName
  | FnItem BodyId
  deriving (Show, Eq)

data Body = Body
  { _bodyParams  :: [Param]
  , _bodyVars    :: IM.IntMap Var
  , _bodyVarIds  :: [VarId]
  , _bodyValue   :: Expr
  }
  deriving (Show, Eq)

instance SourcePosProvider Body where
  _sourcePos = _sourcePos . _bodyValue

data Var = Var
  { _varId    :: VarId
  , _varIdent :: Ident
  }
  deriving (Show, Eq)

instance SourcePosProvider Var where
  _sourcePos = _sourcePos . _varIdent

newtype Param = Param { _paramVarId :: VarId }
  deriving (Show, Eq)

data Expr = Expr
  { _exprSourcePos :: SourcePos
  , _exprKind      :: ExprKind
  }
  deriving (Show, Eq)

instance SourcePosProvider Expr where
  _sourcePos = _exprSourcePos

data ExprKind
  = PathExpr Path
  | LiteralExpr Literal
  | BlockExpr Block
  | UnaryExpr UnOp Expr
  | BinExpr BinOp Expr Expr
  | CallExpr Expr [Expr]
  | WhileExpr Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | AssignExpr VarId Expr
  | ReturnExpr Expr
  deriving (Show, Eq)

data Ident = Ident
  { _identName      :: Text
  , _identSourcePos :: SourcePos
  }
  deriving (Show, Eq)

instance SourcePosProvider Ident where
  _sourcePos = _identSourcePos

data Block = Block
  { _blockExprs     :: [Expr]
  , _blockSourcePos :: SourcePos
  }
  deriving (Show, Eq)

instance SourcePosProvider Block where
  _sourcePos = _blockSourcePos

data Path = Path
  { _pathSourcePos :: SourcePos
  , _pathKind      :: PathKind
  }
  deriving (Show, Eq)

instance SourcePosProvider Path where
  _sourcePos = _pathSourcePos

data PathKind
  = Local VarId
  | Global ItemId
  deriving (Show, Eq)

data UnOp = UnOp
  { _unOpSourcePos :: SourcePos
  , _unOpKind      :: UnOpKind
  }
  deriving (Show, Eq)

data UnOpKind
  = UnNeg
  | UnNot
  deriving (Eq, Show)

data BinOp = BinOp
  { _binOpSourcePos :: SourcePos
  , _binOpKind      :: BinOpKind
  }
  deriving (Show, Eq)

data BinOpKind
  = BinAdd
  | BinAnd
  | BinDiv
  | BinEq
  | BinGt
  | BinGtEq
  | BinLt
  | BinLtEq
  | BinMul
  | BinNeq
  | BinOr
  | BinSEq
  | BinSNeq
  | BinSub
  | BinXor
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Lenses

makeWrapped ''ItemId
makeWrapped ''BodyId
makeWrapped ''VarId

makeLenses ''Module

makeLenses ''Item

makeLenses ''ItemKind
makePrisms ''ItemKind

makeLenses ''Body

makeLenses ''Var

makeLenses ''Param
makeWrapped ''Param

makeLenses ''Expr

makeLenses ''ExprKind
makePrisms ''ExprKind

makeLenses ''Ident

makeLenses ''Block

makeLenses ''Path

makeLenses ''PathKind
makePrisms ''PathKind

makeLenses ''UnOp

makeLenses ''UnOpKind
makePrisms ''UnOpKind

makeLenses ''BinOp

makeLenses ''BinOpKind
makePrisms ''BinOpKind

moduleItem :: ItemId -> Traversal' Module Item
moduleItem (ItemId i) = moduleItems . ix i

moduleBody :: BodyId -> Traversal' Module Body
moduleBody (BodyId i) = moduleBodies . ix i

bodyVar :: VarId -> Traversal' Body Var
bodyVar (VarId i) = bodyVars . ix i
