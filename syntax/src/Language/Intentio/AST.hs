module Language.Intentio.AST
  ( module Language.Intentio.AST
  , ModuleName(..)
  , ItemName(..)
  , unModuleName
  , unItemName
  , getAnn
  , setAnn
  , ann
  )
where

import           Intentio.Prelude

import           Data.Convertible               ( convError )

import           Intentio.Annotated             ( Annotated(..) )

import qualified Language.Intentio.Assembly    as A
import           Language.Intentio.Assembly     ( ModuleName(..)
                                                , ItemName(..)
                                                , unModuleName
                                                , unItemName
                                                )
import           Language.Intentio.Token
import           Language.Intentio.SourcePos    ( HasSourcePos(..)
                                                , SourcePos(..)
                                                )

--------------------------------------------------------------------------------
-- AST data structures

data ModId a = ModId
  { _modIdAnn       :: a
  , _modIdSourcePos :: SourcePos
  , _unModId        :: Text
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (ModId a) where
  _sourcePos = _modIdSourcePos

instance ToJSON a => ToJSON (ModId a)
instance FromJSON a => FromJSON (ModId a)

data ScopeId a = ScopeId
  { _scopeIdAnn       :: a
  , _scopeIdSourcePos :: SourcePos
  , _unScopeId        :: Text
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (ScopeId a) where
  _sourcePos = _scopeIdSourcePos

instance ToJSON a => ToJSON (ScopeId a)
instance FromJSON a => FromJSON (ScopeId a)

data Qid a = Qid
  { _qidAnn       :: a
  , _qidSourcePos :: SourcePos
  , _qidMod       :: Text
  , _qidScope     :: Text
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (Qid a) where
  _sourcePos = _qidSourcePos

instance ToJSON a => ToJSON (Qid a)
instance FromJSON a => FromJSON (Qid a)

data AnyId a
  = Qid' (Qid a)
  | ScopeId' (ScopeId a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (AnyId a) where
  _sourcePos (Qid'     i) = _sourcePos i
  _sourcePos (ScopeId' i) = _sourcePos i

instance Annotated AnyId where
  getAnn (Qid'     i) = getAnn i
  getAnn (ScopeId' i) = getAnn i

  setAnn b (Qid'     i) = Qid' $ setAnn b i
  setAnn b (ScopeId' i) = ScopeId' $ setAnn b i

instance ToJSON a => ToJSON (AnyId a)
instance FromJSON a => FromJSON (AnyId a)

data Module a = Module
  { _moduleAnn       :: a
  , _moduleSourcePos :: SourcePos
  , _moduleName      :: ModuleName
  , _moduleExport    :: Maybe (ExportDecl a)
  , _moduleItems     :: [ItemDecl a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (Module a) where
  _sourcePos = _moduleSourcePos

instance ToJSON a => ToJSON (Module a)
instance FromJSON a => FromJSON (Module a)

instance (Eq a, Show a) => A.Module (Module a) where
  type ItemTy (Module a) = ItemDecl a
  _moduleName  = Language.Intentio.AST._moduleName
  _moduleItems = Language.Intentio.AST._moduleItems

data ItemDecl a = ItemDecl
  { _itemDeclAnn       :: a
  , _itemDeclSourcePos :: SourcePos
  , _itemDeclKind      :: ItemDeclKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (ItemDecl a) where
  _sourcePos = _itemDeclSourcePos

instance ToJSON a => ToJSON (ItemDecl a)
instance FromJSON a => FromJSON (ItemDecl a)

instance (Eq a, Show a) => A.Item (ItemDecl a) where
  _itemName ItemDecl { _itemDeclKind = ImportItemDecl _ } = Nothing
  _itemName ItemDecl { _itemDeclKind = FunItemDecl f } =
    Just . convert $ _funDeclName f

data ItemDeclKind a
  = ImportItemDecl (ImportDecl a)
  | FunItemDecl (FunDecl a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ItemDeclKind a)
instance FromJSON a => FromJSON (ItemDeclKind a)

data ExportDecl a = ExportDecl
  { _exportDeclAnn       :: a
  , _exportDeclSourcePos :: SourcePos
  , _exportDeclItems     :: [ScopeId a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (ExportDecl a) where
  _sourcePos = _exportDeclSourcePos

instance ToJSON a => ToJSON (ExportDecl a)
instance FromJSON a => FromJSON (ExportDecl a)

data ImportDecl a = ImportDecl
  { _importDeclAnn       :: a
  , _importDeclSourcePos :: SourcePos
  , _importDeclKind      :: ImportDeclKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (ImportDecl a) where
  _sourcePos = _importDeclSourcePos

instance ToJSON a => ToJSON (ImportDecl a)
instance FromJSON a => FromJSON (ImportDecl a)

data ImportDeclKind a
  = ImportQid (Qid a)
  | ImportQidAs (Qid a) (ScopeId a)
  | ImportId (ModId a)
  | ImportIdAs (ModId a) (ModId a)
  | ImportAll (ModId a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ImportDeclKind a)
instance FromJSON a => FromJSON (ImportDeclKind a)

data FunDecl a = FunDecl
  { _funDeclAnn       :: a
  , _funDeclSourcePos :: SourcePos
  , _funDeclName      :: ScopeId a
  , _funDeclParams    :: [FunParam a]
  , _funDeclBody      :: FunBody a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (FunDecl a) where
  _sourcePos = _funDeclSourcePos

instance ToJSON a => ToJSON (FunDecl a)
instance FromJSON a => FromJSON (FunDecl a)

newtype FunParam a = FunParam { _funParamId :: ScopeId a }
  deriving (Show, Eq, Generic, HasSourcePos, ToJSON, FromJSON, Functor, Foldable, Traversable)

newtype FunBody a = FunBody { _funBodyBlock :: Block a }
  deriving (Show, Eq, Generic, HasSourcePos, ToJSON, FromJSON, Functor, Foldable, Traversable)

data Stmt a = Stmt
  { _stmtAnn       :: a
  , _stmtSourcePos :: SourcePos
  , _stmtKind      :: StmtKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (Stmt a) where
  _sourcePos = _stmtSourcePos

instance ToJSON a => ToJSON (Stmt a)
instance FromJSON a => FromJSON (Stmt a)

data StmtKind a
  = AssignStmt (ScopeId a) (Expr a)
  | ExprStmt (Expr a)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StmtKind a)
instance FromJSON a => FromJSON (StmtKind a)

data Expr a = Expr
  { _exprAnn       :: a
  , _exprSourcePos :: SourcePos
  , _exprKind      :: ExprKind a
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (Expr a) where
  _sourcePos = _exprSourcePos

instance ToJSON a => ToJSON (Expr a)
instance FromJSON a => FromJSON (Expr a)

data ExprKind a
  = IdExpr (AnyId a)
  | LitExpr (Lit a)
  | BlockExpr (Block a)
  | SuccExpr (Expr a)
  | FailExpr (Expr a)
  | UnExpr (UnOp a) (Expr a)
  | BinExpr (BinOp a) (Expr a) (Expr a)
  | CallExpr (Expr a) [Expr a]
  | WhileExpr (Expr a) (Block a)
  | IfExpr (Expr a) (Block a) (Maybe (Block a))
  | ParenExpr (Expr a)
  | ReturnExpr (Maybe (Expr a))
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ExprKind a)
instance FromJSON a => FromJSON (ExprKind a)

data Block a = Block
  { _blockAnn       :: a
  , _blockSourcePos :: SourcePos
  , _blockStmts     :: [Stmt a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance HasSourcePos (Block a) where
  _sourcePos = _blockSourcePos

instance ToJSON a => ToJSON (Block a)
instance FromJSON a => FromJSON (Block a)

data Lit a = Lit
  { _litAnn       :: a
  , _litSourcePos :: SourcePos
  , _litText      :: Text
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
  | RawStringLit Text
  | NoneLit
  deriving (Show, Eq, Generic)

instance ToJSON LitKind
instance FromJSON LitKind

data UnOp a = UnOp
  { _unOpAnn       :: a
  , _unOpSourcePos :: SourcePos
  , _unOpKind      :: UnOpKind
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (UnOp a)
instance FromJSON a => FromJSON (UnOp a)

instance HasSourcePos (UnOp a) where
  _sourcePos = _unOpSourcePos

data UnOpKind
  = UnNeg
  | UnNot
  deriving (Show, Eq, Generic)

instance ToJSON UnOpKind
instance FromJSON UnOpKind

data BinOp a = BinOp
  { _binOpAnn       :: a
  , _binOpSourcePos :: SourcePos
  , _binOpKind      :: BinOpKind
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (BinOp a)
instance FromJSON a => FromJSON (BinOp a)

instance HasSourcePos (BinOp a) where
  _sourcePos = _binOpSourcePos

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

makeLenses ''ModId
makeLenses ''ScopeId
makeLenses ''Qid
makePrisms ''AnyId
makeLenses ''Module
makeLenses ''ItemDecl
makePrisms ''ItemDeclKind
makeLenses ''ExportDecl
makeLenses ''ImportDecl
makePrisms ''ImportDeclKind
makeLenses ''FunDecl
makeLenses ''FunParam
makeLenses ''FunBody
makeLenses ''Stmt
makePrisms ''StmtKind
makeLenses ''Expr
makePrisms ''ExprKind
makeLenses ''Block
makeLenses ''Lit
makePrisms ''LitKind
makeLenses ''UnOp
makePrisms ''UnOpKind
makeLenses ''BinOp
makePrisms ''BinOpKind

instance Annotated ModId where
  ann = modIdAnn

instance Annotated ScopeId where
  ann = scopeIdAnn

instance Annotated Qid where
  ann = qidAnn

instance Annotated Module where
  ann = moduleAnn

instance Annotated ExportDecl where
  ann = exportDeclAnn

instance Annotated ImportDecl where
  ann = importDeclAnn

instance Annotated ItemDecl where
  ann = itemDeclAnn

instance Annotated FunDecl where
  ann = funDeclAnn

instance Annotated Stmt where
  ann = stmtAnn

instance Annotated Expr where
  ann = exprAnn

instance Annotated Block where
  ann = blockAnn

instance Annotated Lit where
  ann = litAnn

instance Annotated UnOp where
  ann = unOpAnn

instance Annotated BinOp where
  ann = binOpAnn

--------------------------------------------------------------------------------
-- Token/TokenType -> AST tag conversions

instance Convertible TokenType BinOpKind where
  safeConvert TKwAnd  = Right BinAnd
  safeConvert TKwOr   = Right BinOr
  safeConvert TKwXor  = Right BinXor
  safeConvert TOpAdd  = Right BinAdd
  safeConvert TOpDiv  = Right BinDiv
  safeConvert TOpEqEq = Right BinEq
  safeConvert TOpGt   = Right BinGt
  safeConvert TOpGtEq = Right BinGtEq
  safeConvert TOpLt   = Right BinLt
  safeConvert TOpLtEq = Right BinLtEq
  safeConvert TOpMul  = Right BinMul
  safeConvert TOpNeq  = Right BinNeq
  safeConvert TOpSEq  = Right BinSEq
  safeConvert TOpSNeq = Right BinSNeq
  safeConvert TOpSub  = Right BinSub
  safeConvert x       = convError "Not a binary operator" x

instance Convertible Token BinOpKind where
  safeConvert Token { _ty } = safeConvert _ty

instance Convertible TokenType UnOpKind where
  safeConvert TKwNot = Right UnNot
  safeConvert TOpSub = Right UnNeg
  safeConvert x      = convError "Not an unary operator" x

instance Convertible Token UnOpKind where
  safeConvert Token { _ty } = safeConvert _ty

--------------------------------------------------------------------------------
-- Utilities

instance Convertible (ScopeId a) ItemName where
  safeConvert ScopeId { _unScopeId } = Right $ ItemName _unScopeId
