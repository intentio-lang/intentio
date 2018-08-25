module Intentio.Hir
  ( module Intentio.Hir
  , module X
  )
where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M

import qualified Intentio.Compiler             as C
import           Intentio.Diagnostics           ( SourcePos(..)
                                                , HasSourcePos(..)
                                                )

import           Intentio.Compiler             as X
                                                ( ModuleName(..)
                                                , ItemName(..)
                                                )

--------------------------------------------------------------------------------
-- HIR data structures

newtype ItemId = ItemId IM.Key
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

newtype BodyId = BodyId IM.Key
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

newtype VarId = VarId IM.Key
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

data Module = Module
  { _moduleSourcePos  :: SourcePos

  , _moduleName       :: ModuleName

  , _moduleExports    :: [ItemId]

  , _moduleItems      :: IM.IntMap Item
  , _moduleItemIds    :: [ItemId]
  , _moduleItemsNames :: M.Map ItemName ItemId

  , _moduleBodies     :: IM.IntMap Body
  , _moduleBodyIds    :: [BodyId]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Module
instance FromJSON Module

instance HasSourcePos Module where
  _sourcePos = _moduleSourcePos

instance C.Module Module where
  type ItemTy Module = Item
  _moduleName = Intentio.Hir._moduleName
  _moduleItems Module { _moduleItems = i, _moduleItemIds = n } =
    toList $ (\w -> i ^?! ix (w ^. _Wrapped)) <$> n

data Item = Item
  { _itemSourcePos  :: SourcePos
  , _itemId         :: ItemId
  , _itemName       :: Maybe ItemName
  , _itemKind       :: ItemKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

instance HasSourcePos Item where
  _sourcePos = _itemSourcePos

instance C.Item Item where
  _itemName = Intentio.Hir._itemName

data ItemKind
  = ImportItem ModuleName ItemName
  | FnItem BodyId
  deriving (Show, Eq, Generic)

instance ToJSON ItemKind
instance FromJSON ItemKind

data Body = Body
  { _bodyParams  :: [Param]
  , _bodyVars    :: IM.IntMap Var
  , _bodyVarIds  :: [VarId]
  , _bodyValue   :: Expr
  }
  deriving (Show, Eq, Generic)

instance ToJSON Body
instance FromJSON Body

instance HasSourcePos Body where
  _sourcePos = _sourcePos . _bodyValue

data Var = Var
  { _varId    :: VarId
  , _varIdent :: Ident
  }
  deriving (Show, Eq, Generic)

instance ToJSON Var
instance FromJSON Var

instance HasSourcePos Var where
  _sourcePos = _sourcePos . _varIdent

newtype Param = Param { _paramVarId :: VarId }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Expr = Expr
  { _exprSourcePos :: SourcePos
  , _exprKind      :: ExprKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON Expr
instance FromJSON Expr

instance HasSourcePos Expr where
  _sourcePos = _exprSourcePos

data ExprKind
  = PathExpr Path
  | LitExpr Lit
  | BlockExpr Block
  | UnaryExpr UnOp Expr
  | BinExpr BinOp Expr Expr
  | CallExpr Expr [Expr]
  | WhileExpr Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | AssignExpr VarId Expr
  | ReturnExpr Expr
  deriving (Show, Eq, Generic)

instance ToJSON ExprKind
instance FromJSON ExprKind

data Ident = Ident
  { _identSourcePos :: SourcePos
  , _identName      :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Ident
instance FromJSON Ident

instance HasSourcePos Ident where
  _sourcePos = _identSourcePos

data Lit = Lit
  { _litSourcePos :: SourcePos
  , _litKind      :: LitKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON Lit
instance FromJSON Lit

instance HasSourcePos Lit where
  _sourcePos = _litSourcePos

data LitKind
  = IntegerLit Integer
  | FloatLit Double
  | StringLit Text
  | CharLit Char
  | RegexLit Text
  | NoneLit
  deriving (Show, Eq, Generic)

instance ToJSON LitKind
instance FromJSON LitKind

data Block = Block
  { _blockSourcePos :: SourcePos
  , _blockExprs     :: [Expr]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Block
instance FromJSON Block

instance HasSourcePos Block where
  _sourcePos = _blockSourcePos

data Path = Path
  { _pathSourcePos :: SourcePos
  , _pathKind      :: PathKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON Path
instance FromJSON Path

instance HasSourcePos Path where
  _sourcePos = _pathSourcePos

data PathKind
  = Local VarId
  | Global ItemId
  deriving (Show, Eq, Generic)

instance ToJSON PathKind
instance FromJSON PathKind

data UnOp = UnOp
  { _unOpSourcePos :: SourcePos
  , _unOpKind      :: UnOpKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON UnOp
instance FromJSON UnOp

data UnOpKind
  = UnNeg
  | UnNot
  deriving (Show, Eq, Generic)

instance ToJSON UnOpKind
instance FromJSON UnOpKind

data BinOp = BinOp
  { _binOpSourcePos :: SourcePos
  , _binOpKind      :: BinOpKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON BinOp
instance FromJSON BinOp

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
  deriving (Show, Eq, Generic)

instance ToJSON BinOpKind
instance FromJSON BinOpKind

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

makeLenses ''Lit

makeLenses ''LitKind
makePrisms ''LitKind

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

--------------------------------------------------------------------------------
-- Helper accessors

findParamVar :: Body -> Param -> Maybe Var
findParamVar body param = body ^. bodyVars ^? ix i
  where i = param ^. paramVarId . _Wrapped
