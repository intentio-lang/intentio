module Language.Intentio.AST where

import           Intentio.Prelude ()

data Module = Module [ItemDecl]

data ItemDecl = FunDecl QId FunParam FunBody

newtype ModId = ModId Id

data QId = QId ModId Id

data Id = Ident

newtype FunParam = FunParam [Id]

newtype FunBody = FunBody Block

data Expr
  =   BinExpr BinOp Expr Expr
  | BlockExpr Block
  | FunCallExpr Expr [Expr]
  | IdExpr QId
  | IfExpr Expr Block
  | IfElseExpr Expr Block Block
  | LetDeclExpr Id Expr
  | LitExpr Literal
  | LoopExpr Expr Block
  | UnaryExpr UnaryOp Expr
  | ParenExpr Expr

newtype Block = Block [Expr]

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
