module Language.Intentio.AST where

import           Intentio.Prelude

import           Data.Convertible               ( convError )

import           Intentio.Compiler.Assembly     ( Module(..)
                                                , ModuleName(..)
                                                , Item(..)
                                                , ItemName(..)
                                                )
import           Intentio.Diagnostics           ( HasSourcePos(..) )

import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- AST data structures

data ModuleSource = ModuleSource {
    _moduleSourceName :: Text,
    _moduleSourceItems :: [ItemDecl]
  }
  deriving (Show, Eq, Generic)

instance HasSourcePos ModuleSource where
  _sourcePos _ = undefined -- TODO:

instance ToJSON ModuleSource
instance FromJSON ModuleSource

instance Module ModuleSource where
  type ItemTy ModuleSource = ItemDecl
  _moduleName = ModuleName . _moduleSourceName
  _moduleItems = _moduleSourceItems

data ItemDecl
  = FunDecl {
    _itemDeclName :: ScopeId,
    _funDeclParams :: FunParams,
    _funDeclBody :: FunBody
  }
  deriving (Show, Eq, Generic)

instance HasSourcePos ItemDecl where
  _sourcePos _ = undefined -- TODO:

instance ToJSON ItemDecl
instance FromJSON ItemDecl

instance Item ItemDecl where
  _itemName = Just . ItemName . (\(ScopeId n) -> n) . _itemDeclName

newtype ModId = ModId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ScopeId = ScopeId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AnyId
  = Qid ModId ScopeId
  | Id ScopeId
  deriving (Show, Eq, Generic)

instance ToJSON AnyId
instance FromJSON AnyId

newtype FunParams = FunParams [FunParam]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype FunParam = FunParam ScopeId
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype FunBody = FunBody Block
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Expr
  = BinExpr BinOp Expr Expr
  | BlockExpr Block
  | FunCallExpr Expr FunArgs
  | IdExpr AnyId
  | IfExpr { _ifCondition :: Expr, _ifBody :: Block }
  | IfElseExpr { _ifCondition :: Expr, _ifBody :: Block, _elseBody :: Block }
  | LetDeclExpr { _letDeclName :: ScopeId, _letDeclVal :: Expr }
  | LitExpr Literal
  | WhileExpr { _whileCondition :: Expr, _whileBody :: Block }
  | UnaryExpr UnaryOp Expr
  | ParenExpr Expr
  | ReturnExpr (Maybe Expr)
  deriving (Show, Eq, Generic)

instance ToJSON Expr
instance FromJSON Expr

newtype FunArgs = FunArgs [Expr]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Block = Block [Expr]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Literal
  = Integer Text
  | Float Text
  | String Text
  | CharString Text
  | RawString Text
  | RegexString Text
  deriving (Show, Eq, Generic)

instance ToJSON Literal
instance FromJSON Literal

data BinOp
  = BinAdd
  | BinAnd
  | BinDiv
  | BinEqEq
  | BinGt
  | BinGtEq
  | BinLt
  | BinLtEq
  | BinMul
  | BinNeq
  | BinOr
  | BinSub
  deriving (Show, Eq, Generic)

instance ToJSON BinOp
instance FromJSON BinOp

data UnaryOp
  = UnaryAdd
  | UnaryNot
  | UnarySub
  deriving (Show, Eq, Generic)

instance ToJSON UnaryOp
instance FromJSON UnaryOp

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''AnyId
makeLenses ''Block
makeLenses ''Expr
makeLenses ''FunArgs
makeLenses ''FunBody
makeLenses ''FunParam
makeLenses ''FunParams
makeLenses ''ItemDecl
makeLenses ''ModId
makeLenses ''ModuleSource
makeLenses ''ScopeId

--------------------------------------------------------------------------------
-- Token/TokenType -> AST tag conversions

instance Convertible TokenType BinOp where
  safeConvert TKwAnd  = Right BinAnd
  safeConvert TKwOr   = Right BinOr
  safeConvert TOpAdd  = Right BinAdd
  safeConvert TOpSub  = Right BinSub
  safeConvert TOpMul  = Right BinMul
  safeConvert TOpDiv  = Right BinDiv
  safeConvert TOpEqEq = Right BinEqEq
  safeConvert TOpNeq  = Right BinNeq
  safeConvert TOpLt   = Right BinLt
  safeConvert TOpLtEq = Right BinLtEq
  safeConvert TOpGt   = Right BinGt
  safeConvert TOpGtEq = Right BinGtEq
  safeConvert x = convError "Not a binary operator" x

instance Convertible Token BinOp where
  safeConvert Token{_ty} = safeConvert _ty

instance Convertible TokenType UnaryOp where
  safeConvert TKwNot = Right UnaryNot
  safeConvert TOpAdd = Right UnaryAdd
  safeConvert TOpSub = Right UnarySub
  safeConvert x = convError "Not an unary operator" x

instance Convertible Token UnaryOp where
  safeConvert Token{_ty} = safeConvert _ty

instance Convertible Token Literal where
  safeConvert Token{_ty=TInteger, _text}     = Right $ Integer _text
  safeConvert Token{_ty=TFloat, _text}       = Right $ Float _text
  safeConvert Token{_ty=TString, _text}      = Right $ String _text
  safeConvert Token{_ty=TCharString, _text}  = Right $ CharString _text
  safeConvert Token{_ty=TRawString, _text}   = Right $ RawString _text
  safeConvert Token{_ty=TRegexString, _text} = Right $ RegexString _text
  safeConvert x = convError "Not a literal" x
