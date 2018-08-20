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

import           Intentio.Prelude        hiding ( Prefix
                                                , many
                                                , mod
                                                , try
                                                )

import           Text.Megaparsec                ( (<?>)
                                                , between
                                                , eof
                                                , many
                                                , optional
                                                , sepEndBy
                                                , try
                                                )
import qualified Text.Megaparsec               as M
import           Text.Megaparsec.Expr           ( Operator
                                                  ( InfixL
                                                  , InfixR
                                                  , Prefix
                                                  )
                                                , makeExprParser
                                                )

import           Intentio.Compiler              ( ModuleName )

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
  :: ModuleName -- ^ Name of module.
  -> FilePath   -- ^ Name of source file.
  -> Text       -- ^ Input for parser.
  -> Either ParserError ModuleSource
parseModule modName = M.parse (mod modName)

parseItemDecl
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserError ItemDecl
parseItemDecl = M.parse itemDecl

parseExpr
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserError Expr
parseExpr = M.parse expr

--------------------------------------------------------------------------------
-- Core parser productions

mod :: Text -> Parser ModuleSource
mod _moduleSourceName = do
  _moduleSourceItems <- many itemDecl
  _                  <- eof
  return ModuleSource {..}

itemDecl :: Parser ItemDecl
itemDecl = funDecl

expr :: Parser Expr
expr = opexpr

--------------------------------------------------------------------------------
-- Identifiers

modId :: Parser ModId
modId = ModId . (^. text) <$> ident

scopeId :: Parser ScopeId
scopeId = ScopeId . (^. text) <$> ident

anyId :: Parser AnyId
anyId = try qid <|> try id
 where
  id  = Id <$> scopeId
  qid = do
    m <- modId
    _ <- tok TOpColon
    s <- scopeId
    return $ Qid m s

--------------------------------------------------------------------------------
-- Item declarations

funDecl :: Parser ItemDecl
funDecl = do
  _              <- tok TKwFun
  _itemDeclName  <- scopeId
  _funDeclParams <- params
  _funDeclBody   <- body
  return FunDecl {..}
 where
  params    = FunParams <$> parens paramList
  paramList = sepEndBy param comma
  param     = FunParam <$> scopeId
  body      = FunBody <$> block

--------------------------------------------------------------------------------
-- Expressions

opexpr :: Parser Expr
opexpr = makeExprParser term table
 where
  table =
    [ [prefix TKwNot, prefix TOpSub, prefix TOpAdd]
    , [infixL TOpMul, infixL TOpDiv]
    , [infixL TOpAdd, infixL TOpSub]
    , [ infixL TOpEqEq
      , infixL TOpNeq
      , infixL TOpLtEq
      , infixL TOpGtEq
      , infixL TOpLt
      , infixL TOpGt
      ]
    , [infixL TKwAnd]
    , [infixL TKwOr]
    ]

  prefix tokTy = Prefix $ UnaryExpr <$> (convert <$> tok tokTy)
  infixL tokTy = InfixL $ BinExpr <$> (convert <$> tok tokTy)
  infixR tokTy = InfixR $ BinExpr <$> (convert <$> tok tokTy)

term :: Parser Expr
term =
  try letdeclexpr
    <|> try whileexpr
    <|> try ifelseexpr
    <|> try ifexpr
    <|> try returnexpr
    <|> try blockexpr
    <|> try funcallexpr
    <|> try parenexpr
    <|> try litexpr
    <|> try idexpr

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

parenexpr :: Parser Expr
parenexpr = ParenExpr <$> parens expr

funcallexpr :: Parser Expr
funcallexpr = do
  c <- try idexpr <|> try parenexpr
  p <- parens argList
  return $ FunCallExpr c p
  where argList = FunArgs <$> sepEndBy expr comma

idexpr :: Parser Expr
idexpr = IdExpr <$> anyId

litexpr :: Parser Expr
litexpr = LitExpr . convert <$> literal <?> "literal"

--------------------------------------------------------------------------------
-- Utilities

block :: Parser Block
block = Block <$> braced exprList where exprList = sepEndBy expr semi

braced :: Parser a -> Parser a
braced = between (tok TOpLBrace) (tok TOpRBrace)

parens :: Parser a -> Parser a
parens = between (tok TOpLParen) (tok TOpRParen)

semi :: Parser Token
semi = tok TOpSemicolon

comma :: Parser Token
comma = tok TOpComma
