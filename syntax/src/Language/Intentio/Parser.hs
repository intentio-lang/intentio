module Language.Intentio.Parser
  ( Parser
  , ParserError
  , parseModule
  , parseItemDecl
  , parseStmt
  , mod
  , itemDecl
  , importDecl
  , exportDecl
  , stmt
  )
where

import           Intentio.Prelude        hiding ( Prefix
                                                , many
                                                , mod
                                                , try
                                                , id
                                                , assign
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

import           Language.Intentio.Assembly     ( ModuleName(..) )
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
parseModule (ModuleName modName) = M.parse (mod modName)

parseItemDecl
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserError ItemDecl
parseItemDecl = M.parse itemDecl

parseStmt
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserError Stmt
parseStmt = M.parse stmt

--------------------------------------------------------------------------------
-- Core parser productions

mod :: Text -> Parser ModuleSource
mod _moduleSourceName = do
  _moduleSourceExport   <- optional exportDecl
  _moduleSourceImports  <- many importDecl
  _moduleSourceItems    <- many itemDecl
  eof
  return ModuleSource { .. }

itemDecl :: Parser ItemDecl
itemDecl = funDecl

stmt :: Parser Stmt
stmt = 
  (AssignStmt <$> try assign) 
  <|> (ExprStmt <$> try opexpr)

--------------------------------------------------------------------------------
-- Identifiers

modId :: Parser ModId
modId = ModId . (^. text) <$> ident

scopeId :: Parser ScopeId
scopeId = ScopeId . (^. text) <$> ident

anyId :: Parser AnyId
anyId = try qid <|> try id

qid :: Parser AnyId
qid = do
  m <- modId
  tok TOpColon
  s <- scopeId
  return $ Qid m s

id :: Parser AnyId
id  = Id <$> scopeId

--------------------------------------------------------------------------------
-- Import - export declarations
-- test, czy najpierw nie zeżre id

exportDecl :: Parser ExportDecl
exportDecl = do
  tok TKwExport
  f  <- funNames
  return $ ExportDecl f
  where 
    funNames    = ExportItems <$> parens exportItems
    exportItems = sepEndBy exportItem comma
    exportItem  = ExportItem <$> scopeId

importDecl :: Parser ImportDecl
importDecl = 
  try importqidas 
  <|> try importqid 
  <|> try importidas 
  <|> try importid

importqidas :: Parser ImportDecl
importqidas = do
  tok TKwImport
  m  <- modId
  tok TOpColon
  s  <- scopeId
  tok TKwAs
  n  <- scopeId
  return $ ImportQidAs m s n

importqid :: Parser ImportDecl
importqid = do
  tok TKwImport
  m  <- modId
  tok TOpColon
  s  <- scopeId
  return $ ImportQid m s

importidas :: Parser ImportDecl
importidas = do
  tok TKwImport
  s  <- scopeId
  tok TKwAs
  n  <- scopeId
  return $ ImportIdAs s n

importid :: Parser ImportDecl
importid = do
  tok TKwImport
  s  <- scopeId
  return $ ImportId s

--------------------------------------------------------------------------------
-- Item declarations

funDecl :: Parser ItemDecl
funDecl = do
  tok TKwFun
  _itemDeclName  <- scopeId
  _funDeclParams <- params
  _funDeclBody   <- body
  return FunDecl { .. }
 where
  params    = FunParams <$> parens paramList
  paramList = sepEndBy param comma
  param     = FunParam <$> scopeId
  body      = FunBody <$> block

--------------------------------------------------------------------------------
-- Assignments 

assign :: Parser Assign
assign = do
  _name   <- scopeId
  tok TOpEq
  _val    <- expr
  return Assign {..}

--------------------------------------------------------------------------------
-- Expressions

expr :: Parser Expr
expr = opexpr

opexpr :: Parser Expr
opexpr = makeExprParser term table
 where
  table =
    [ [prefix TKwNot, prefix TOpSub, prefix TOpAdd]
    , [infixL TOpMul, infixL TOpDiv]
    , [infixL TOpAdd, infixL TOpSub]
    , [infixL TOpEqEqEq, infixL TOpNeqEq]
    , [ infixL TOpEqEq
      , infixL TOpNeq
      , infixL TOpLtEq
      , infixL TOpGtEq
      , infixL TOpLt
      , infixL TOpGt
      ]
    , [infixL TKwXor]
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
  tok TKwLet
  _letDeclName <- scopeId
  tok TOpEq
  _letDeclVal  <- expr
  return LetDeclExpr { .. }

whileexpr :: Parser Expr
whileexpr = do
  tok TKwWhile
  _whileCondition <- expr
  _whileBody      <- block
  return WhileExpr { .. }

ifelseexpr :: Parser Expr
ifelseexpr = do
  tok TKwIf
  i <- expr
  t <- block
  tok TKwElse
  e <- block
  return $ IfElseExpr i t e

ifexpr :: Parser Expr
ifexpr = do
  tok TKwIf
  e <- expr
  b <- block
  return $ IfExpr e b

returnexpr :: Parser Expr
returnexpr = do
  tok TKwReturn
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
