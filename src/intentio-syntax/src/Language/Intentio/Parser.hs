module Language.Intentio.Parser where

  import           Intentio.Prelude        hiding ( many )
  
  import           Text.Megaparsec         hiding ( parse )
  import qualified Text.Megaparsec               as M
  import qualified Language.Intentio.AST         as A
  
  import qualified Language.Intentio.Token       as I
  
  import           Language.Intentio.Lexer        ( tok
                                                  , literal
                                                  , ident
                                                  )
  
  type Parser = Parsec Void Text
  type ParserError = ParseError Text Void
  
  parse
    :: String -- ^ Name of source file.
    -> Text   -- ^ Input for parser.
    -> Either ParserError A.Module
  parse = M.parse program   -- ??
  
  
  program :: Parser A.Module
  program = A.Module <$> many item
  
  
  item :: Parser A.ItemDecl
  item = do
    i <- qid
    p <- funparams
    b <- funbody
    return $ A.FunDecl i p b
  
  qid :: Parser A.QId
  qid = do
    m <- modid
    tok I.OpColon
    i <- id
    return $ A.QId m i
  
  modid :: Parser A.ModId
  modid = do
    A.ModId <$> id
  
  id :: Parser A.Id
  id = lexer ident
  
  funparams :: Parser A.FunParam
  funparams = A.FunParam <$> braced paramList
    where paramList = many (id <* semi)
  
  funbody :: Parser A.FunBody
  funbody = do
    A.FunBody <$> block
  
  expr :: Parser A.Expr
  expr =
    try letdeclexpr
      <|> try loopexpr
      <|> try ifelseexpr
      <|> try ifexpr
      <|> try blockexpr
      <|> try binexpr
      <|> try unaryexpr
      <|> try parenexpr
      <|> try funcallexpr
      <|> try idexpr
      <|> try litexpr
  
  letdeclexpr :: Parser A.Expr
  letdeclexpr = do
    tok I.KwLet
    i <- id
    e <- expr
    return $ A.LetDeclExpr i e
  
  loopexpr :: Parser A.Expr
  loopexpr = do
    tok I.KwWhile
    e <- expr
    b <- block
    return $ A.LoopExpr e b
  
  ifelseexpr :: Parser A.Expr
  ifelseexpr = do
    tok I.KwIf
    i <- expr
    t <- block
    tok I.KwElse
    e <- block
    return $ A.IfElseExpr i t e
  
  ifexpr :: Parser A.Expr
  ifexpr = do
    tok I.KwIf --am am
    e <- expr
    b <- block
    return $ A.IfExpr e b
  
  blockexpr :: Parser A.Expr
  blockexpr = A.BlockExpr <$> block
  
  binexpr :: Parser A.Expr
  binexpr = do
    o <- binop
    l <- expr
    r <- expr
    return $ A.BinExpr o l r
  
  unaryexpr :: Parser A.Expr
  unaryexpr = do
    o <- unaryop
    e <- expr
    return $ A.UnaryExpr o e
  
  parenexpr :: Parser A.Expr
  parenexpr = A.ParenExpr <$> paren expr
  
  funcallexpr :: Parser A.Expr
  funcallexpr = do
    c <- expr
    p <- paren paramList
    return $ A.FunCallExpr c p
    where paramList = many (expr <* semi)
  
  idexpr :: Parser A.Expr
  idexpr = A.IdExpr <$> qid
  
  litexpr :: Parser A.Expr
  litexpr = A.LitExpr <$> lit -- ??
  
  block :: Parser A.Block
  block = A.Block <$> braced exprList where exprList = many (expr <* coma)
  
  braced :: Parser a -> Parser a
  braced = between (tok I.OpLBrace) (tok I.OpRBrace)
  
  paren :: Parser a -> Parser a
  paren = between (tok I.OpLParen) (tok I.OpRParen)
  
  semi :: Parser I.Token
  semi = tok I.OpSemicolon
  
  coma :: Parser I.Token
  coma = tok I.OpCom
  
  lit :: Parser I.Token
  lit = tok literal
  
  binop :: Parser A.BinOp
  binop = undefined
  
  unaryop :: Parser A.UnaryOp
  unaryop = undefined
  