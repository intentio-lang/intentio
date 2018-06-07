module Language.Intentio.AST where

import           Intentio.Prelude

import           Data.Convertible               ( convError )

import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- AST data structures

newtype Assembly = Assembly {
    _assemblyModules :: [Module]
  }
  deriving (Eq, Show)

newtype Module = Module {
      _moduleItems :: [ItemDecl]
   }
   deriving (Eq, Show)

data ItemDecl
  = FunDecl {
    _funDeclName :: ScopeId,
    _funDeclParams :: FunParams,
    _funDeclBody :: FunBody
  }
  deriving (Eq, Show)

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
  | BinSub
  | BinMul
  | BinDiv
  | BinEq
  | BinNeq
  | BinLt
  | BinLtEq
  | BinGt
  | BinGtEq
  deriving (Eq, Show)

data UnaryOp
  = UnaryAdd
  | UnarySub
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''AnyId
makeLenses ''Assembly
makeLenses ''Block
makeLenses ''Expr
makeLenses ''FunArgs
makeLenses ''FunBody
makeLenses ''FunParam
makeLenses ''FunParams
makeLenses ''ItemDecl
makeLenses ''ModId
makeLenses ''Module
makeLenses ''ScopeId

--------------------------------------------------------------------------------
-- Token/TokenType -> AST tag conversions

instance Convertible TokenType BinOp where
  safeConvert TOpAdd  = Right BinAdd
  safeConvert TOpSub  = Right BinSub
  safeConvert TOpMul  = Right BinMul
  safeConvert TOpDiv  = Right BinDiv
  safeConvert TOpEq   = Right BinEq
  safeConvert TOpNeq  = Right BinNeq
  safeConvert TOpLt   = Right BinLt
  safeConvert TOpLtEq = Right BinLtEq
  safeConvert TOpGt   = Right BinGt
  safeConvert TOpGtEq = Right BinGtEq
  safeConvert x = convError "Not a binary operator" x

instance Convertible Token BinOp where
  safeConvert Token{_ty} = safeConvert _ty

instance Convertible TokenType UnaryOp where
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
