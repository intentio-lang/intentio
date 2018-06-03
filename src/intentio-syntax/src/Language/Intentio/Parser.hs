module Language.Intentio.Parser where

import qualified Language.Intentio.Token as I
import qualified Language.Intentio.AST as A
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M

parse
  :: String -- ^ Name of source file.
  -> [I.Token]   -- ^ Input for parser.
  -> Either ParserError A.AST
parse p = M.parse program

data Module = Module [ItemDecl]

data ItemDecl = FunDecl QId [FunParam] FunBody

newtype ModId = ModId Id

data QId = QId ModId Id

newtype FunParam = FunParam [Id]

newtype FunBody = FunBody [Expr]

data Expr =   BinExpr Expr Expr 
            | FunCall Expr [Expr]
            | IdExpr QId
            | LetDecl Id Expr
            | UnaryExpr Expr


