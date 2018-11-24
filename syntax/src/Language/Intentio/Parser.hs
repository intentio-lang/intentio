module Language.Intentio.Parser
  ( Parser
  , ParserError
  , ParserErrorBundle
  , lex
  , lexTest
  , parseModule
  , parseItemDecl
  , parseStmt
  , parseItemDecls
  , parseStmts
  )
where

import           Intentio.Prelude        hiding ( Prefix
                                                , exponent
                                                , fail
                                                , many
                                                , mod
                                                , none
                                                , option
                                                , some
                                                , succ
                                                , try
                                                , un
                                                )
import qualified Intentio.Prelude

import           Control.Monad.Combinators.Expr ( Operator(InfixL, Prefix)
                                                , makeExprParser
                                                )
import           Data.Char                      ( isLower
                                                , digitToInt
                                                )
import qualified Data.Bimap                    as BM
import           Data.Scientific                ( scientific )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( Parsec
                                                , ParseError
                                                , ParseErrorBundle
                                                , (<?>)
                                                , anySingleBut
                                                , between
                                                , count
                                                , eof
                                                , getSourcePos
                                                , many
                                                , notFollowedBy
                                                , oneOf
                                                , option
                                                , optional
                                                , parse
                                                , parseTest
                                                , satisfy
                                                , sepEndBy
                                                , some
                                                , try
                                                , unexpected
                                                )
import qualified Text.Megaparsec               as M
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , char'
                                                , digitChar
                                                , hexDigitChar
                                                , letterChar
                                                , octDigitChar
                                                , space1
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.Intentio.Assembly     ( ModuleName(..) )
import           Language.Intentio.AST
import           Language.Intentio.SourcePos    ( SourcePos(..)
                                                , HasSourcePos(..)
                                                )
import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- Parser monad

type Parser = Parsec Void Text
type ParserError = ParseError Text Void
type ParserErrorBundle = ParseErrorBundle Text Void

--------------------------------------------------------------------------------
-- Lexer-only parsing entry points

-- | Run lexer over input text. Returns either lexer error or list of tokens.
lex
  :: String -- Parser Name of source file.
  -> Text   -- ^ Input for lexer.
  -> Either ParserErrorBundle [Token]
lex = parse programLex

-- | Run lexer over input text and print the results to standard output.
-- Useful for testing.
lexTest
  :: Text -- ^ Input for lexer.
  -> IO ()
lexTest = parseTest programLex

--------------------------------------------------------------------------------
-- Parser entry-points

parseModule
  :: ModuleName -- ^ Name of module.
  -> FilePath   -- ^ Name of source file.
  -> Text       -- ^ Input for parser.
  -> Either ParserErrorBundle (Module ())
parseModule = M.parse . mod

parseItemDecl
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserErrorBundle (ItemDecl ())
parseItemDecl = M.parse itemDecl

parseStmt
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserErrorBundle (Stmt ())
parseStmt = M.parse stmt

parseItemDecls
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserErrorBundle [ItemDecl ()]
parseItemDecls = M.parse (some itemDecl <* eof)

parseStmts
  :: FilePath -- ^ Name of source file.
  -> Text     -- ^ Input for parser.
  -> Either ParserErrorBundle [Stmt ()]
parseStmts = M.parse (stmts <* eof)

--------------------------------------------------------------------------------
-- Lexer productions

-- | Parse whole program as a list of tokens.
programLex :: Parser [Token]
programLex = between sc eof $ many anyTok

-- | Parse any valid token.
anyTok :: Parser Token
anyTok = literalLex <|> ident <|> anyKeyword <|> anyOperator

-- | Parse token of given type.
tok :: TokenType -> Parser Token
tok TFloat        = fst <$> float
tok TIdent        = ident
tok TInteger      = fst <$> integer
tok TKwAbstract   = tokKw TKwAbstract
tok TKwAnd        = tokKw TKwAnd
tok TKwAs         = tokKw TKwAs
tok TKwBreak      = tokKw TKwBreak
tok TKwCase       = tokKw TKwCase
tok TKwConst      = tokKw TKwConst
tok TKwContinue   = tokKw TKwContinue
tok TKwDo         = tokKw TKwDo
tok TKwElse       = tokKw TKwElse
tok TKwEnum       = tokKw TKwEnum
tok TKwEval       = tokKw TKwEval
tok TKwExport     = tokKw TKwExport
tok TKwExtern     = tokKw TKwExtern
tok TKwFail       = tokKw TKwFail
tok TKwFun        = tokKw TKwFun
tok TKwIf         = tokKw TKwIf
tok TKwImpl       = tokKw TKwImpl
tok TKwImport     = tokKw TKwImport
tok TKwIn         = tokKw TKwIn
tok TKwIs         = tokKw TKwIs
tok TKwLoop       = tokKw TKwLoop
tok TKwModule     = tokKw TKwModule
tok TKwNone       = tokKw TKwNone
tok TKwNot        = tokKw TKwNot
tok TKwOr         = tokKw TKwOr
tok TKwReturn     = tokKw TKwReturn
tok TKwStruct     = tokKw TKwStruct
tok TKwSucc       = tokKw TKwSucc
tok TKwTrait      = tokKw TKwTrait
tok TKwType       = tokKw TKwType
tok TKwUnderscore = tokKw TKwUnderscore
tok TKwWhere      = tokKw TKwWhere
tok TKwWhile      = tokKw TKwWhile
tok TKwXor        = tokKw TKwXor
tok TKwYield      = tokKw TKwYield
tok TOpAdd        = tokOp TOpAdd
tok TOpColon      = tokOp TOpColon
tok TOpColonEq    = tokOp TOpColonEq
tok TOpComma      = tokOp TOpComma
tok TOpDiv        = tokOp TOpDiv
tok TOpDollar     = tokOp TOpDollar
tok TOpEq         = tokOp TOpEq
tok TOpEqEq       = tokOp TOpEqEq
tok TOpGt         = tokOp TOpGt
tok TOpGtEq       = tokOp TOpGtEq
tok TOpLBrace     = tokOp TOpLBrace
tok TOpLBracket   = tokOp TOpLBracket
tok TOpLParen     = tokOp TOpLParen
tok TOpLt         = tokOp TOpLt
tok TOpLtEq       = tokOp TOpLtEq
tok TOpLtSub      = tokOp TOpLtSub
tok TOpMul        = tokOp TOpMul
tok TOpNeq        = tokOp TOpNeq
tok TOpPercent    = tokOp TOpPercent
tok TOpRBrace     = tokOp TOpRBrace
tok TOpRBracket   = tokOp TOpRBracket
tok TOpRParen     = tokOp TOpRParen
tok TOpSemicolon  = tokOp TOpSemicolon
tok TOpSEq        = tokOp TOpSEq
tok TOpSNeq       = tokOp TOpSNeq
tok TOpSub        = tokOp TOpSub
tok TRawString    = fst <$> rawstring
tok TString       = fst <$> string'

--------------------------------------------------------------------------------
-- Token productions

-- | Parse an identifier.
ident :: Parser Token
ident = identT >>= mkt TIdent <?> "identifier"

-- | Parse an identifier.
identT :: Parser Text
identT = (lexeme . try) (p >>= nonReserved) <?> "identifier"
 where
  p :: Parser Text
  p = identStart >:> many identContinue

  nonReserved :: Text -> Parser Text
  nonReserved w
    | isKeyword w = Intentio.Prelude.fail $ "Illegal identifier: " <> toS w
    | otherwise   = return w

  isKeyword :: Text -> Bool
  isKeyword = flip BM.member keywords

-- | Parse any valid keyword.
anyKeyword :: Parser Token
anyKeyword = anyReserved keywords <?> "keyword"

-- | Parse any valid operator.
anyOperator :: Parser Token
anyOperator = anyReserved operators <?> "operator"

-- | Tokenize any valid literal.
literalLex :: Parser Token
literalLex =
  (fst <$> float)
    <|> (fst <$> integer)
    <|> (fst <$> string')
    <|> (fst <$> rawstring)
    <|> none

-- | Parse any valid literal.
literal :: Parser (Lit ())
literal = par <?> "literal"
 where
  par = do
    sp <- srcPos
    l1 sp float FloatLit
      <|> l1 sp integer   IntegerLit
      <|> l1 sp string'   StringLit
      <|> l1 sp rawstring RawStringLit
      <|> l0 sp none NoneLit

  l1 _litSourcePos p k = do
    let _litAnn = ()
    (t, v) <- p
    let _litText = t ^. text
    let _litKind = k v
    return Lit { .. }

  l0 _litSourcePos p _litKind = do
    let _litAnn = ()
    _litText <- view text <$> p
    return Lit { .. }

-- | Parse none literal
none :: Parser Token
none = tok TKwNone <?> "none literal"

-- | Parse any valid integer literal.
integer :: Parser (Token, Integer)
integer = (lexeme . try) grammar >>= mktt TInteger <?> "integer literal"
 where
  grammar     = try binary <|> try octal <|> try hexadecimal <|> decimalNum
  binary      = gen 'b' binaryNum
  octal       = gen 'o' octalNum
  hexadecimal = gen 'x' hexadecimalNum

  gen c p = do
    z      <- char '0'
    k      <- char' c
    (t, v) <- p
    return (z <| k <| t, v)

-- | Parse any valid floating-point literal.
float :: Parser (Token, Scientific)
float = (lexeme . try) grammar >>= mktt TFloat <?> "floating-point literal"
 where
  grammar = do
    (ct, cv)         <- decimalNum
    (dt, et, dv, ev) <- pDotExp <|> pExpOnly
    let v = scientific (cv * 10 ^ countDigits dv + dv) (ev - countDigits dv)
    return (ct <> dt <> et, v)

  pDotExp = do
    (dt, dv) <- pDot
    (et, ev) <- optExp
    return (dt, et, dv, ev)

  pDot = do
    d        <- char '.'
    (vt, vv) <- decimalNum
    return (d <| vt, vv)

  optExp   = option ("", 0) exponent

  pExpOnly = do
    (et, ev) <- exponent
    return ("", et, 0, ev)

decimalNum :: Num a => Parser (Text, a)
decimalNum = parseNum <$> p <?> "decimal digits"
  where p = digitChar >:> many (digitChar <|> char '_')

binaryNum :: Num a => Parser (Text, a)
binaryNum = parseNum <$> p <?> "binary digits"
 where
  p            = binDigitChar >:> many (binDigitChar <|> char '_')
  binDigitChar = oneOf ['0', '1'] <?> "binary digit"

octalNum :: Num a => Parser (Text, a)
octalNum = parseNum <$> p <?> "octal digits"
  where p = octDigitChar >:> many (octDigitChar <|> char '_')

hexadecimalNum :: Num a => Parser (Text, a)
hexadecimalNum = parseNum <$> p <?> "hexadecimal digits"
  where p = hexDigitChar >:> many (hexDigitChar <|> char '_')

parseNum :: (StringConv s String, StringConv s Text, Num n) => s -> (Text, n)
parseNum s = (toS s, fromIntegral r)
 where
  r = foldl' step 0 (toS s :: String)

  step a '_' = a
  step a c   = 10 * a + digitToInt c

exponent :: Num a => Parser (Text, a)
exponent = do
  e           <- T.singleton <$> oneOf ['e', 'E']
  sign        <- option "" $ T.singleton <$> oneOf ['+', '-']
  underscores <- toS <$> many (char '_')
  (vt, vv)    <- decimalNum
  let fsign = if sign == "-" then negate else id
  return (e <> sign <> underscores <> vt, fsign vv)

-- | Parse valid regular string literal.
string' :: Parser (Token, Text)
string' = genString istring' TString "string"

-- | Parse valid raw string literal.
rawstring :: Parser (Token, Text)
rawstring = genString irawstring TRawString "raw string"

genString :: Parser (Text, Text) -> TokenType -> String -> Parser (Token, Text)
genString parser tt lbl = (lexeme . try) grammar >>= mktt tt <?> lbl
 where
  grammar = do
    prefix <- stringprefix
    (t, v) <- parser
    let t' = prefix <> t
    v' <- applyMods (toS prefix) v
    return (t', v')

  applyMods []         v = return v
  applyMods ('x' : ps) v = applyMods ps v
  applyMods (p   : _ ) _ = unexpected $ M.Tokens (p :| [])

istring' :: Parser (Text, Text)
istring' = do
  l <- string "\""
  v <- T.concat <$> many strchr
  r <- string "\""
  -- FIXME: Process escape sequences
  return (l <> v <> r, v)

irawstring :: Parser (Text, Text)
irawstring = do
  r      <- char 'r'
  (t, v) <- rawstring' 0
  return (r <| t, v)
 where
  rawstring' :: Int -> Parser (Text, Text)
  rawstring' n = pQuot n <|> pHash n

  pQuot n = delim '"' $ rawstring'' n
  pHash n = delim '#' $ rawstring' (n + 1)

  delim c p = do
    l      <- char c
    (t, v) <- p
    r      <- char c
    return ((l <| t) |> r, v)

  rawstring'' :: Int -> Parser (Text, Text)
  rawstring'' n = many rwsany <&> toS <&> \t -> (t, t)
   where
    rwsany = try (anySingleBut '"') <|> try (char '"' <* notFollowedBy hashes)
    hashes = count n $ char '#'

stringprefix :: Parser Text
stringprefix = option "" stringmod

stringmod :: Parser Text
stringmod = toS <$> some (satisfy isStringModChar) <?> "string modifier"
  where isStringModChar c = c /= 'r' && isLower c

strchr :: Parser Text
strchr =
  try escseq
    <|> (T.singleton <$> satisfy (\c -> c /= '"' && c /= '\\'))
    <?> "string character or escape sequence"

escseq :: Parser Text
escseq = try charesc <|> try asciiesc <|> try unicodeesc
 where
  charesc = do
    slash <- char '\\'
    code  <- oneOf ['\'', '"', 'n', 'r', 't', '\\', '0']
    return $ toS [slash, code]

  asciiesc   = string "\\x" <:< hexDigitChar <:< hexDigitChar

  unicodeesc = do
    prefix <- string "\\u{"
    val    <- many (hexDigitChar <|> char '_')
    suffix <- string "}"
    return $ prefix <> toS val <> suffix

--------------------------------------------------------------------------------
-- Core parser productions

mod :: ModuleName -> Parser (Module ())
mod _moduleName = do
  let _moduleAnn = ()
  _moduleSourcePos <- srcPos
  between sc eof $ do
    _moduleExport <- optional exportDecl
    _moduleItems  <- many itemDecl
    return Module { .. }

--------------------------------------------------------------------------------
-- Identifiers

genId :: (() -> SourcePos -> Text -> i) -> Parser i
genId f = f <$> pure () <*> srcPos <*> identT

modId :: Parser (ModId ())
modId = genId ModId <?> "module name"

scopeId :: Parser (ScopeId ())
scopeId = genId ScopeId <?> "identifier"

qid :: Parser (Qid ())
qid =
  (Qid <$> pure () <*> srcPos <*> (identT <* tok TOpColon) <*> identT)
    <?> "qualified identifier"

anyId :: Parser (AnyId ())
anyId = try (Qid' <$> qid) <|> (ScopeId' <$> scopeId)

exportDecl :: Parser (ExportDecl ())
exportDecl =
  ExportDecl
    <$> pure ()
    <*> srcPos
    <*> (tok TKwExport *> exportItems)
    <?> "export declaration"
  where exportItems = parens $ sepEndBy scopeId comma

--------------------------------------------------------------------------------
-- Item declarations

itemDecl :: Parser (ItemDecl ())
itemDecl = importItem <|> externFunItem <|> funItem

importItem :: Parser (ItemDecl ())
importItem = item' (ImportItemDecl <$> importDecl) <?> "import declaration"

modGlob :: Parser (ModId ())
modGlob = modId <* (tok TOpColon *> tok TOpMul)

data ImportFirstId a = ImportFirstQid (Qid a) | ImportFirstMod (ModId a) | ImportFirstGlob (ModId a)

importDecl :: Parser (ImportDecl ())
importDecl = do
  pos <- srcPos
  void $ tok TKwImport
  k <- pFirstId >>= \case
    ImportFirstGlob i -> return $ ImportAll i
    ImportFirstQid  i -> pRename scopeId >>= \case
      Just r  -> return $ ImportQidAs i r
      Nothing -> return $ ImportQid i
    ImportFirstMod i -> pRename modId >>= \case
      Just r  -> return $ ImportIdAs i r
      Nothing -> return $ ImportId i
  return $ ImportDecl () pos k
 where
  pFirstId =
    try (ImportFirstGlob <$> modGlob)
      <|> try (ImportFirstQid <$> qid)
      <|> (ImportFirstMod <$> modId)

  pRename p = optional (tok TKwAs *> p)

funItem :: Parser (ItemDecl ())
funItem = item' (FunItemDecl <$> funDecl) <?> "function item"

funDecl :: Parser (FunDecl ())
funDecl = p <?> "function declaration"
 where
  p = do
    let _funDeclAnn = ()
    _funDeclSourcePos <- srcPos
    tok TKwFun
    _funDeclName   <- scopeId
    _funDeclParams <- params
    _funDeclBody   <- FunBody <$> block
    return FunDecl { .. }

externFunItem :: Parser (ItemDecl ())
externFunItem =
  item' (ExternFunItemDecl <$> externFunDecl) <?> "extern function item"

externFunDecl :: Parser (ExternFunDecl ())
externFunDecl = p <?> "extern function declaration"
 where
  p = do
    let _externFunDeclAnn = ()
    _externFunDeclSourcePos <- srcPos
    _externFunDeclCallConv  <- externCallConv
    tok TKwFun
    _externFunDeclName   <- scopeId
    _externFunDeclParams <- params
    return ExternFunDecl { .. }

params :: Parser [FunParam ()]
params = parens $ sepEndBy param comma

param :: Parser (FunParam ())
param = FunParam <$> scopeId

externCallConv :: Parser CallConv
externCallConv =
  tok TKwExtern *> ((lexeme . try $ string "\"intentio\"") $> IntentioCallConv)

--------------------------------------------------------------------------------
-- Statements

stmt :: Parser (Stmt ())
stmt = try assignStmt <|> exprStmt

stmts :: Parser [Stmt ()]
stmts = sepEndBy stmt semi <?> "statements"

assignStmt :: Parser (Stmt ())
assignStmt =
  stmt' (AssignStmt <$> (scopeId <* tok TOpEq) <*> expr)
    <?> "assignment statement"

exprStmt :: Parser (Stmt ())
exprStmt = stmt' (ExprStmt <$> expr) <?> "expression statement"

--------------------------------------------------------------------------------
-- Expressions

expr :: Parser (Expr ())
expr = opExpr

opExpr :: Parser (Expr ())
opExpr = makeExprParser termWithCall table <?> "expression"
 where
  table =
    [ [prefix TKwNot, prefix TOpSub]
    , [infixL TOpMul, infixL TOpDiv]
    , [infixL TOpAdd, infixL TOpSub]
    , [ infixL TOpSEq
      , infixL TOpSNeq
      , infixL TOpEqEq
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

  prefix tokTy = Prefix $ un (_sourcePos ()) tokTy
  infixL tokTy = InfixL $ bin (_sourcePos ()) tokTy
  -- infixR tokTy = InfixR $ bin (_sourcePos ()) tokTy

  un :: SourcePos -> TokenType -> Parser (Expr () -> Expr ())
  un pos tokTy = do
    o <- UnOp <$> pure () <*> srcPos <*> (convert <$> tok tokTy)
    return $ \e -> Expr () pos (UnExpr o e)

  bin :: SourcePos -> TokenType -> Parser (Expr () -> Expr () -> Expr ())
  bin pos tokTy = do
    o <- BinOp <$> pure () <*> srcPos <*> (convert <$> tok tokTy)
    return $ \l r -> Expr () pos (BinExpr o l r)

termWithCall :: Parser (Expr ())
termWithCall = try callExpr <|> term

term :: Parser (Expr ())
term =
  litExpr
    <|> blockExpr
    <|> succExpr
    <|> failExpr
    <|> whileExpr
    <|> ifExpr
    <|> parenExpr
    <|> returnExpr
    <|> idExpr

idExpr :: Parser (Expr ())
idExpr = expr' (IdExpr <$> anyId)

litExpr :: Parser (Expr ())
litExpr = expr' (LitExpr <$> literal)

blockExpr :: Parser (Expr ())
blockExpr = expr' (BlockExpr <$> block) <?> "block"

succExpr :: Parser (Expr ())
succExpr = expr' (SuccExpr <$> (tok TKwSucc *> expr) <?> "succ expression")

failExpr :: Parser (Expr ())
failExpr = expr' (FailExpr <$> (tok TKwFail *> expr) <?> "fail expression")

callExpr :: Parser (Expr ())
callExpr = expr' (CallExpr <$> term <*> args) <?> "function call"
  where args = parens (sepEndBy expr comma)

whileExpr :: Parser (Expr ())
whileExpr = expr' p <?> "while expression"
  where p = WhileExpr <$> (tok TKwWhile *> expr) <*> block

ifExpr :: Parser (Expr ())
ifExpr = expr' p <?> "if expression"
 where
  p = IfExpr <$> (tok TKwIf *> expr) <*> block <*> optional e
  e = tok TKwElse *> block

parenExpr :: Parser (Expr ())
parenExpr = expr' (ParenExpr <$> parens expr) <?> "parenthesized expression"

returnExpr :: Parser (Expr ())
returnExpr =
  expr' (ReturnExpr <$> (tok TKwReturn *> optional expr))
    <?> "return expression"

--------------------------------------------------------------------------------
-- Utilities

mkt :: TokenType -> Text -> Parser Token
mkt _ty _text = return Token { .. }

mktt :: TokenType -> (Text, a) -> Parser (Token, a)
mktt _ty (_text, a) = return (Token { .. }, a)

sc :: Parser ()
sc = L.space space1 lineComment empty
  where lineComment = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identStart :: Parser Char
identStart = letterChar <|> char '_'

identContinue :: Parser Char
identContinue = alphaNumChar <|> char '_' <|> char '\''

anyReserved :: BM.Bimap Text TokenType -> Parser Token
anyReserved = try . BM.fold (\s t p -> (symbol s >>= mkt t) <|> p) empty

tokKw :: TokenType -> Parser Token
tokKw tt = notFollowedBy ident *> tokReserved keywords tt

tokOp :: TokenType -> Parser Token
tokOp = tokReserved operators

tokReserved :: BM.Bimap Text TokenType -> TokenType -> Parser Token
tokReserved m t = (try . symbol) s >>= mkt t where s = m BM.!> t

keywords :: BM.Bimap Text TokenType
keywords = BM.fromList
  [ ("abstract", TKwAbstract)
  , ("and"     , TKwAnd)
  , ("as"      , TKwAs)
  , ("break"   , TKwBreak)
  , ("case"    , TKwCase)
  , ("const"   , TKwConst)
  , ("continue", TKwContinue)
  , ("do"      , TKwDo)
  , ("else"    , TKwElse)
  , ("enum"    , TKwEnum)
  , ("eval"    , TKwEval)
  , ("export"  , TKwExport)
  , ("extern"  , TKwExtern)
  , ("fail"    , TKwFail)
  , ("fun"     , TKwFun)
  , ("if"      , TKwIf)
  , ("impl"    , TKwImpl)
  , ("import"  , TKwImport)
  , ("in"      , TKwIn)
  , ("is"      , TKwIs)
  , ("loop"    , TKwLoop)
  , ("module"  , TKwModule)
  , ("none"    , TKwNone)
  , ("not"     , TKwNot)
  , ("or"      , TKwOr)
  , ("return"  , TKwReturn)
  , ("struct"  , TKwStruct)
  , ("succ"    , TKwSucc)
  , ("trait"   , TKwTrait)
  , ("type"    , TKwType)
  , ("where"   , TKwWhere)
  , ("while"   , TKwWhile)
  , ("xor"     , TKwXor)
  , ("yield"   , TKwYield)
  , ("_"       , TKwUnderscore)
  ]

operators :: BM.Bimap Text TokenType
operators = BM.fromList
  [ ("+"  , TOpAdd)
  , ("-"  , TOpSub)
  , ("*"  , TOpMul)
  , ("/"  , TOpDiv)
  , ("("  , TOpLParen)
  , (")"  , TOpRParen)
  , ("["  , TOpLBracket)
  , ("]"  , TOpRBracket)
  , ("{"  , TOpLBrace)
  , ("}"  , TOpRBrace)
  , (","  , TOpComma)
  , (":"  , TOpColon)
  , (";"  , TOpSemicolon)
  , ("="  , TOpEq)
  , ("==" , TOpEqEq)
  , ("===", TOpSEq)
  , ("<"  , TOpLt)
  , ("<=" , TOpLtEq)
  , (">"  , TOpGt)
  , (">=" , TOpGtEq)
  , ("!=" , TOpNeq)
  , ("!==", TOpSNeq)
  , (":=" , TOpColonEq)
  , ("<-" , TOpLtSub)
  , ("$"  , TOpDollar)
  , ("%"  , TOpPercent)
  ]

infixl 6 <:<
(<:<) :: StringConv t Text => Parser t -> Parser Char -> Parser Text
(<:<) l r = snoc <$> (toS <$> l) <*> r

infixr 6 >:>
(>:>) :: StringConv t Text => Parser Char -> Parser t -> Parser Text
(>:>) l r = cons <$> l <*> (toS <$> r)

srcPos :: Parser SourcePos
srcPos = _sourcePos <$> getSourcePos

item' :: Parser (ItemDeclKind ()) -> Parser (ItemDecl ())
item' p = ItemDecl <$> pure () <*> srcPos <*> p

stmt' :: Parser (StmtKind ()) -> Parser (Stmt ())
stmt' p = Stmt <$> pure () <*> srcPos <*> p

expr' :: Parser (ExprKind ()) -> Parser (Expr ())
expr' p = Expr <$> pure () <*> srcPos <*> p

block :: Parser (Block ())
block = Block <$> pure () <*> srcPos <*> p where p = braced stmts <?> "block"

braced :: Parser a -> Parser a
braced = between (tok TOpLBrace) (tok TOpRBrace)

parens :: Parser a -> Parser a
parens = between (tok TOpLParen) (tok TOpRParen)

semi :: Parser Token
semi = tok TOpSemicolon

comma :: Parser Token
comma = tok TOpComma

countDigits :: Integer -> Int
countDigits = go 1 . abs
 where
  go c x | x < 10    = c
         | otherwise = go (c + 1) (x `div` 10)
