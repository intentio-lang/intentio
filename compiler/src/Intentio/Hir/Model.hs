module Intentio.Hir.Model
  ( module Intentio.Hir.Model
  , module X
  , getAnn
  , setAnn
  , ann
  )
where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M

import           Intentio.Annotated             ( Annotated(..) )
import qualified Intentio.Compiler             as C
import           Intentio.Compiler             as X
                                                ( ModuleName(..)
                                                , ItemName(..)
                                                )
import           Intentio.Diagnostics           ( SourcePos(..)
                                                , HasSourcePos(..)
                                                )
import           Intentio.Util.NodeId           ( NodeId )
import           Language.Intentio.AST         as X
                                                ( UnOp(..)
                                                , UnOpKind(..)
                                                , BinOp(..)
                                                , BinOpKind(..)
                                                , unOpAnn
                                                , unOpSourcePos
                                                , unOpKind
                                                , binOpAnn
                                                , binOpSourcePos
                                                , binOpKind
                                                , _UnNeg
                                                , _UnNot
                                                , _BinAdd
                                                , _BinAnd
                                                , _BinDiv
                                                , _BinEq
                                                , _BinGt
                                                , _BinGtEq
                                                , _BinLt
                                                , _BinLtEq
                                                , _BinMul
                                                , _BinNeq
                                                , _BinOr
                                                , _BinSEq
                                                , _BinSNeq
                                                , _BinSub
                                                , _BinXor
                                                )

--------------------------------------------------------------------------------
-- HIR data structures

newtype ItemId = ItemId { _unItemId :: IM.Key }
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

instance Convertible NodeId ItemId where
  safeConvert = Right . ItemId . fromEnum

newtype BodyId = BodyId { _unBodyId :: IM.Key }
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

instance Convertible NodeId BodyId where
  safeConvert = Right . BodyId . fromEnum

newtype VarId = VarId { _unVarId :: IM.Key }
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Generic, ToJSON, FromJSON)

instance Convertible NodeId VarId where
  safeConvert = Right . VarId . fromEnum

data Module a = Module
  { _moduleAnn        :: a

  , _moduleSourcePos  :: SourcePos

  , _moduleName       :: ModuleName

  , _moduleExports    :: HashSet ItemName
  , _moduleImports    :: HashSet (ModuleName, ItemName)

  , _moduleItems      :: IM.IntMap (Item a)
  , _moduleItemIds    :: [ItemId]
  , _moduleItemsNames :: M.Map ItemName ItemId

  , _moduleBodies     :: IM.IntMap (Body a)
  , _moduleBodyIds    :: [BodyId]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Module a)
instance FromJSON a => FromJSON (Module a)

instance HasSourcePos (Module a) where
  _sourcePos = _moduleSourcePos

instance (Eq a, Show a) => C.Module (Module a) where
  type ItemTy (Module a) = Item a
  _moduleName = Intentio.Hir.Model._moduleName
  _moduleItems Module { _moduleItems = i, _moduleItemIds = n } =
    toList $ (\w -> i ^?! ix (_unItemId w)) <$> n

data Item a = Item
  { _itemAnn        :: a
  , _itemSourcePos  :: SourcePos
  , _itemId         :: ItemId
  , _itemName       :: Maybe ItemName
  , _itemKind       :: ItemKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Item a)
instance FromJSON a => FromJSON (Item a)

instance HasSourcePos (Item a) where
  _sourcePos = _itemSourcePos

instance (Eq a, Show a) => C.Item (Item a) where
  _itemName = Intentio.Hir.Model._itemName

newtype ItemKind a
  = FnItem BodyId
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ItemKind a)
instance FromJSON a => FromJSON (ItemKind a)

data Body a = Body
  { _bodyAnn     :: a
  , _bodyId      :: BodyId
  , _bodyParams  :: [Param a]
  , _bodyVars    :: IM.IntMap (Var a)
  , _bodyVarIds  :: [VarId]
  , _bodyValue   :: Expr a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Body a)
instance FromJSON a => FromJSON (Body a)

instance HasSourcePos (Body a) where
  _sourcePos = _sourcePos . _bodyValue

data Var a = Var
  { _varAnn       :: a
  , _varSourcePos :: SourcePos
  , _varId        :: VarId
  , _varName      :: Text
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Var a)
instance FromJSON a => FromJSON (Var a)

instance HasSourcePos (Var a) where
  _sourcePos = _varSourcePos

newtype Param a = Param { _paramVarId :: VarId }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable, ToJSON, FromJSON)

data Expr a = Expr
  { _exprAnn       :: a
  , _exprSourcePos :: SourcePos
  , _exprKind      :: ExprKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Expr a)
instance FromJSON a => FromJSON (Expr a)

instance HasSourcePos (Expr a) where
  _sourcePos = _exprSourcePos

data ExprKind a
  = PathExpr (Path a)
  | LitExpr (Lit a)
  | BlockExpr (Block a)
  | SuccExpr (Expr a)
  | FailExpr (Expr a)
  | UnExpr (UnOp a) (Expr a)
  | BinExpr (BinOp a) (Expr a) (Expr a)
  | CallExpr (Expr a) [Expr a]
  | WhileExpr (Expr a) (Expr a)
  | IfExpr (Expr a) (Expr a) (Maybe (Expr a))
  | AssignExpr VarId (Expr a)
  | ReturnExpr (Expr a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ExprKind a)
instance FromJSON a => FromJSON (ExprKind a)

data Lit a = Lit
  { _litAnn       :: a
  , _litSourcePos :: SourcePos
  , _litKind      :: LitKind
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Lit a)
instance FromJSON a => FromJSON (Lit a)

instance HasSourcePos (Lit a) where
  _sourcePos = _litSourcePos

data LitKind
  = IntegerLit Integer
  | FloatLit Scientific
  | StringLit Text
  | NoneLit
  deriving (Show, Eq, Generic)

instance ToJSON LitKind
instance FromJSON LitKind

data Block a = Block
  { _blockAnn       :: a
  , _blockSourcePos :: SourcePos
  , _blockExprs     :: [Expr a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Block a)
instance FromJSON a => FromJSON (Block a)

instance HasSourcePos (Block a) where
  _sourcePos = _blockSourcePos

data Path a = Path
  { _pathAnn       :: a
  , _pathSourcePos :: SourcePos
  , _pathKind      :: PathKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Path a)
instance FromJSON a => FromJSON (Path a)

instance HasSourcePos (Path a) where
  _sourcePos = _pathSourcePos

data PathKind a
  = ToVar VarId
  | ToItem ModuleName ItemName
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (PathKind a)
instance FromJSON a => FromJSON (PathKind a)

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''ItemId
makeLenses ''BodyId
makeLenses ''VarId
makeLenses ''Module
makeLenses ''Item
makePrisms ''ItemKind
makeLenses ''Body
makeLenses ''Var
makeLenses ''Param
makeLenses ''Expr
makePrisms ''ExprKind
makeLenses ''Lit
makePrisms ''LitKind
makeLenses ''Block
makeLenses ''Path
makePrisms ''PathKind

moduleItem :: ItemId -> Traversal' (Module a) (Item a)
moduleItem (ItemId i) = moduleItems . ix i

moduleBody :: BodyId -> Traversal' (Module a) (Body a)
moduleBody (BodyId i) = moduleBodies . ix i

bodyVar :: VarId -> Traversal' (Body a) (Var a)
bodyVar (VarId i) = bodyVars . ix i

instance Annotated Module where
  ann = moduleAnn

instance Annotated Item where
  ann = itemAnn

instance Annotated Body where
  ann = bodyAnn

instance Annotated Var where
  ann = varAnn

instance Annotated Expr where
  ann = exprAnn

instance Annotated Lit where
  ann = litAnn

instance Annotated Block where
  ann = blockAnn

instance Annotated Path where
  ann = pathAnn

--------------------------------------------------------------------------------
-- Helper accessors

findParamVar :: Body a -> Param a -> Maybe (Var a)
findParamVar body param = body ^? (bodyVars . ix i)
  where i = param ^. paramVarId . unVarId
