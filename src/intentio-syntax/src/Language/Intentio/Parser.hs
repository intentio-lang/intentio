module Language.Intentio.Parser
  ( Parser
  , ParserError
  , parseModule
  , parseItemDecl
  , parseExpr
  , mod
  , itemDecl
  , expr
  )
where

import           Intentio.Prelude        hiding ( many
                                                , mod
                                                , try
                                                )

import           Text.Megaparsec                ( (<?>)
                                                , between
                                                , dbg
                                                , many
                                                , optional
                                                , sepBy
                                                , sepEndBy
                                                , try
                                                )
import qualified Text.Megaparsec               as M

import           Language.Intentio.AST
import           Language.Intentio.Lexer        ( Parser
                                                , ParserError
                                                , ident
                                                , literal
                                                , tok
                                                )
import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- Parser entry-points

parseModule
  :: String -- ^ Name of source file.
  -> Text   -- ^ Input for parser.
  -> Either ParserError Module
parseModule = M.parse mod

parseItemDecl
  :: String -- ^ Name of source file.
  -> Text   -- ^ Input for parser.
  -> Either ParserError ItemDecl
parseItemDecl = M.parse itemDecl

parseExpr
  :: String -- ^ Name of source file.
  -> Text   -- ^ Input for parser.
  -> Either ParserError Expr
parseExpr = M.parse expr

--------------------------------------------------------------------------------
-- Core parser productions

mod :: Parser Module
mod = do
  _moduleItems <- many itemDecl
  return Module {..}

itemDecl :: Parser ItemDecl
itemDecl = funDecl

expr :: Parser Expr
expr =
  try letdeclexpr
    <|> try whileexpr
    <|> try ifelseexpr
    <|> try ifexpr
    <|> try returnexpr
    <|> try blockexpr
    -- <|> try binexpr
    -- <|> try unaryexpr
    <|> try parenexpr
    -- <|> try funcallexpr
    <|> try litexpr
    <|> try idexpr

--------------------------------------------------------------------------------
-- Identifiers

modId :: Parser ModId
modId = ModId . (^. text) <$> ident

scopeId :: Parser ScopeId
scopeId = ScopeId . (^. text) <$> ident

anyId :: Parser AnyId
anyId = try (Qid <$> modId <*> scopeId) <|> try (Id <$> scopeId)

--------------------------------------------------------------------------------
-- Item declarations

funDecl :: Parser ItemDecl
funDecl = do
  _              <- tok TKwFun
  _funDeclName   <- scopeId
  _funDeclParams <- params
  _funDeclBody   <- body
  return FunDecl {..}
 where
  params    = FunParams <$> parenthesized paramList
  paramList = sepEndBy param comma
  param     = FunParam <$> scopeId
  body      = FunBody <$> block

--------------------------------------------------------------------------------
-- Expressions

letdeclexpr :: Parser Expr
letdeclexpr = do
  _            <- tok TKwLet
  _letDeclName <- scopeId
  _            <- tok TOpEq
  _letDeclVal  <- expr
  return LetDeclExpr {..}

whileexpr :: Parser Expr
whileexpr = do
  _               <- tok TKwWhile
  _whileCondition <- expr
  _whileBody      <- block
  return WhileExpr {..}

ifelseexpr :: Parser Expr
ifelseexpr = do
  _ <- tok TKwIf
  i <- expr
  t <- block
  _ <- tok TKwElse
  e <- block
  return $ IfElseExpr i t e

ifexpr :: Parser Expr
ifexpr = do
  _ <- tok TKwIf
  e <- expr
  b <- block
  return $ IfExpr e b

returnexpr :: Parser Expr
returnexpr = do
  _ <- tok TKwReturn
  e <- optional expr
  return $ ReturnExpr e

blockexpr :: Parser Expr
blockexpr = BlockExpr <$> block

unaryexpr :: Parser Expr
unaryexpr = do
  o <- convert <$> unaryop
  e <- expr
  return $ UnaryExpr o e
 where
  unaryop :: Parser Token
  unaryop = try (tok TOpAdd) <|> try (tok TOpSub)

parenexpr :: Parser Expr
parenexpr = ParenExpr <$> parenthesized expr

funcallexpr :: Parser Expr
funcallexpr = do
  c <- expr
  p <- parenthesized argList
  return $ FunCallExpr c p
  where argList = FunArgs <$> many (expr <* semi)

idexpr :: Parser Expr
idexpr = IdExpr <$> anyId

litexpr :: Parser Expr
litexpr = LitExpr . convert <$> literal <?> "literal"

--------------------------------------------------------------------------------
-- Binary expressions

binexpr :: Parser Expr
binexpr = do
  o <- convert <$> binop
  l <- expr
  r <- expr
  return $ BinExpr o l r
 where
  binop :: Parser Token
  binop =
    try (tok TOpAdd)
      <|> try (tok TOpSub)
      <|> try (tok TOpMul)
      <|> try (tok TOpDiv)
      <|> try (tok TOpEqEq)
      <|> try (tok TOpLt)
      <|> try (tok TOpLtEq)
      <|> try (tok TOpGt)
      <|> try (tok TOpGtEq)

--------------------------------------------------------------------------------
-- Utilities

block :: Parser Block
block = Block <$> braced exprList where exprList = sepEndBy expr semi

braced :: Parser a -> Parser a
braced = between (tok TOpLBrace) (tok TOpRBrace)

parenthesized :: Parser a -> Parser a
parenthesized = between (tok TOpLParen) (tok TOpRParen)

semi :: Parser Token
semi = tok TOpSemicolon

comma :: Parser Token
comma = tok TOpComma
