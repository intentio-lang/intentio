module Language.Intentio.AST where

import           Intentio.Prelude

import           Data.Convertible               ( convError )

import           Intentio.Compiler.Assembly     ( Module(..)
                                                , ModuleName(..)
                                                , Item(..)
                                                , ItemName(..)
                                                )

import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- AST data structures

data ModuleSource = ModuleSource {
    _moduleSourceName :: Text,
    _moduleSourceItems :: [ItemDecl]
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

instance Item ItemDecl where
  _itemName = ItemName . (\(ScopeId n) -> n) . _itemDeclName

newtype ModId = ModId Text
  deriving (Eq, Show)

newtype ScopeId = ScopeId Text
  deriving (Eq, Show)

data AnyId
  = Qid ModId ScopeId
  | Id ScopeId
  deriving (Eq, Show)

newtype FunParams = FunParams [FunParam]
  deriving (Eq, Show)

newtype FunParam = FunParam ScopeId
  deriving (Eq, Show)

newtype FunBody = FunBody Block
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype FunArgs = FunArgs [Expr]
  deriving (Eq, Show)

newtype Block = Block [Expr]
  deriving (Eq, Show)

data Literal
  = Integer Text
  | Float Text
  | String Text
  | CharString Text
  | RawString Text
  | RegexString Text
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data UnaryOp
  = UnaryAdd
  | UnaryNot
  | UnarySub
  deriving (Eq, Show)

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
