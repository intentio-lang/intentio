module Language.Intentio.Parser where

  import           Intentio.Prelude        hiding ( many
                                                  , try   
                                                  )
  
  import           Text.Megaparsec         hiding ( parse )
  import qualified Text.Megaparsec               as M
  import qualified Language.Intentio.AST         as A
  
  import qualified Language.Intentio.Token       as I
  
  import           Language.Intentio.Lexer        ( tok
                                                  , literal
                                                  , ident
                                                  )
  
  type Lexer = Parsec Void Text
  type ParserError = ParseError Text Void
  
  parse
    :: String -- ^ Name of source file.
    -> Text   -- ^ Input for parser.
    -> Either LexerError A.Module
  parse = M.parse program
  
  
  program :: Lexer A.Module
  program = A.Module <$> many item
  
  item :: Lexer A.ItemDecl
  item = do
    i <- qid
    p <- funparams
    b <- funbody
    return $ A.FunDecl i p b

  qid :: Lexer A.QId
  qid = try qidmod <|> try id
  
  qidmod :: Lexer A.QId
  qidmod = do
    m <- modid
    tok I.OpColon
    i <- id
    return $ A.QId m i
  
  modid :: Lexer A.ModId
  modid = do
    A.ModId <$> id
  
  id :: Lexer A.Id
  id = lexer ident
  
  funparams :: Lexer A.FunParam
  funparams = A.FunParam <$> braced paramList
    where paramList = many (id <* semi)
  
  funbody :: Lexer A.FunBody
  funbody = do
    A.FunBody <$> block
  
  expr :: Lexer A.Expr
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
  
  letdeclexpr :: Lexer A.Expr
  letdeclexpr = do
    tok I.KwLet
    i <- id
    e <- expr
    return $ A.LetDeclExpr i e
  
  loopexpr :: Lexer A.Expr
  loopexpr = do
    tok I.KwWhile
    e <- expr
    b <- block
    return $ A.LoopExpr e b
  
  ifelseexpr :: Lexer A.Expr
  ifelseexpr = do
    tok I.KwIf
    i <- expr
    t <- block
    tok I.KwElse
    e <- block
    return $ A.IfElseExpr i t e
  
  ifexpr :: Lexer A.Expr
  ifexpr = do
    tok I.KwIf --am am
    e <- expr
    b <- block
    return $ A.IfExpr e b
  
  blockexpr :: Lexer A.Expr
  blockexpr = A.BlockExpr <$> block
  
  binexpr :: Lexer A.Expr
  binexpr = do
    o <- binop
    l <- expr
    r <- expr
    return $ A.BinExpr o l r
  
  unaryexpr :: Lexer A.Expr
  unaryexpr = do
    o <- unaryop
    e <- expr
    return $ A.UnaryExpr o e
  
  parenexpr :: Lexer A.Expr
  parenexpr = A.ParenExpr <$> paren expr
  
  funcallexpr :: Lexer A.Expr
  funcallexpr = do
    c <- expr
    p <- paren paramList
    return $ A.FunCallExpr c p
    where paramList = many (expr <* semi)
  
  idexpr :: Lexer A.Expr
  idexpr = A.IdExpr <$> qid
  
  litexpr :: Lexer A.Expr
  litexpr = A.LitExpr literal
  
  block :: Lexer A.Block
  block = A.Block <$> braced exprList where exprList = many (expr <* coma)
  
  braced :: Lexer a -> Lexer a
  braced = between (tok I.OpLBrace) (tok I.OpRBrace)
  
  paren :: Lexer a -> Lexer a
  paren = between (tok I.OpLParen) (tok I.OpRParen)
  
  semi :: Lexer I.Token
  semi = tok I.OpSemicolon
  
  coma :: Lexer I.Token
  coma = tok I.OpComa
  
  lit :: Lexer I.Token
  lit = literal
  
  binop :: Lexer I.Token
  binop = 
    try (tok I.OpAdd)
    <|> try (tok I.OpSub)
    <|> try (tok I.OpMul)
    <|> try (tok I.OpDiv)
    <|> try (tok I.OpEqEq)
    <|> try (tok I.OpLt)
    <|> try (tok I.OpLtEq)
    <|> try (tok I.OpGt)
    <|> try (tok I.OpGtEq)
  
  unaryop :: Lexer I.Token
  unaryop = 
    try (tok I.OpAdd)
    <|> (try tok I.OpSub)

