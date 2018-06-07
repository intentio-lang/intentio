module Language.Intentio.AST where

import           Intentio.Prelude

import           Language.Intentio.Token        ( Token(..)
                                                , TokenType(..)
                                                )

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
  | BinNEq
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

instance Convertible Token BinOp where
  safeConvert Token{_ty} = safeConvert _ty

instance Convertible TokenType UnaryOp where

instance Convertible Token UnaryOp where
  safeConvert Token{_ty} = safeConvert _ty

instance Convertible Token Literal where
