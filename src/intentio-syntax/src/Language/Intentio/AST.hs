module Language.Intentio.AST where

import           Intentio.Prelude ()

data Module = Module [ItemDecl]

data ItemDecl = FunDecl QId [FunParam] FunBody

newtype ModId = ModId Id

data QId = QId ModId Id

data Id = Ident

newtype FunParam = FunParam [Id]

newtype FunBody = FunBody [Expr]

data Expr
  =   BinExpr BinOp Expr Expr
  | BlockExpr [Expr]
  | FunCallExpr Expr [Expr]
  | IdExpr QId
  | IfExpr Expr [Expr]
  | IfElseExpr Expr [Expr] [Expr]
  | LetDeclExpr Id Expr
  | LitExpr Literal
  | LoopExpr Expr [Expr]
  | UnaryExpr UnaryOp Expr
  | ParenExpr Expr

data Literal
  = CharStr
  | Float
  | Integer
  | RawStr
  | RegStr
  | Str

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

data UnaryOp
  = UnaryAdd
  | UnarySub
