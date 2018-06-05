module Language.Intentio.Parser where

import           Intentio.Prelude        hiding ( many )

import           Text.Megaparsec         hiding ( parse )
import qualified Text.Megaparsec               as M
import qualified Language.Intentio.AST         as A

import qualified Language.Intentio.Token       as I

type Parser = Parsec Void [I.Token]
type ParserError = ParseError I.Token Void

parse
  :: String -- ^ Name of source file.
  -> [I.Token]   -- ^ Input for parser.
  -> Either ParserError A.Module
parse = M.parse program


program :: Parser A.Module
program = A.Module <$> many item


item :: Parser A.ItemDecl
item = undefined

expr :: Parser A.Expr
expr = undefined

block :: Parser A.Block
block = A.Block <$> braced exprList
  where exprList = many (expr <* semi)

ifexpr :: Parser A.Expr
ifexpr = do
  tok I.KwIf --am am
  e <- expr
  b <- block
  return $ A.IfExpr e b



-- int :: Parser Expr
-- int = do
--   n <- integer
--   return $ Float (fromInteger n)

-- ifStmt :: Parser Stmt
-- ifStmt = do
--   ifword "if"
--   cond  <- Expr
--   rbrace "{"
--   body <- [Expr]
--   rbrace "{"
--   return (If cond body)

tok :: I.TokenType -> Parser I.Token
tok = undefined

-- satisfy f = token testChar Nothing
--   where
--     testChar x =
--       if f x
--         then Right x
--         else Left (pure (Tokens (x:|[])), Set.empty)

braced :: Parser a -> Parser a
braced = between (tok I.OpLBrace) (tok I.OpRBrace)

semi :: Parser I.Token
semi = tok I.OpSemicolon