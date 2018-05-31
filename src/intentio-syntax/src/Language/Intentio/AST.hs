module Language.Intentio.AST
where

import           Intentio.Prelude

import           Data.Text                      ( Text )

type Qid = Text

data Expr
  = Float Double
  | BinOp Operator Expr Expr
  | Var Text
  | Call Qid [Expr]
  | Function Qid [Expr] Expr
  | Extern Qid [Expr]
  deriving (Eq, Ord, Show)

data Operator
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
