module Language.Intentio.Lexer
  ( Parser
  , ParserError
  , lex
  , lexTest
  , lexTest'
  , program
  , anyTok
  , tok
  , anyKeyword
  , anyOperator
  , ident
  , literal
  , integer
  , float
  , anyString
  , string'
  , regexstring
  , rawstring
  )
where

import           Intentio.Prelude        hiding ( many
                                                , option
                                                , some
                                                , try
                                                , exponent
                                                , none
                                                , succ
                                                , fail
                                                )
import qualified Intentio.Prelude              as P (
                                                fail
                                                )

import           Data.Char                      ( isLower )
import qualified Data.Bimap                    as BM
import qualified Data.Text                     as T

import           Text.Megaparsec                ( MonadParsec
                                                , Parsec
                                                , ParseError
                                                , (<?>)
                                                , count
                                                , eof
                                                , many
                                                , notFollowedBy
                                                , option
                                                , parse
                                                , parseTest
                                                , parseTest'
                                                , some
                                                , try
                                                )
import qualified Text.Megaparsec               as MP
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , digitChar
                                                , hexDigitChar
                                                , letterChar
                                                , notChar
                                                , octDigitChar
                                                , oneOf
                                                , satisfy
                                                , space1
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.Intentio.Token

--------------------------------------------------------------------------------
-- Parser monad

type Parser = Parsec Void Text

type ParserError = ParseError (MP.Token Text) Void

--------------------------------------------------------------------------------
-- Lexer-only parsing entry points

-- | Run lexer over input text. Returns either lexer error or list of tokens.
lex
  :: String -- Parser Name of source file.
  -> Text   -- ^ Input for lexer.
  -> Either ParserError [Token]
lex = parse program

-- | Run lexer over input text and print the results to standard output.
-- Useful for testing.
lexTest
  :: Text -- ^ Input for lexer.
  -> IO ()
lexTest = parseTest program

-- | A version of 'lexTest' that also prints offending line in parse errors.
lexTest'
  :: Text -- ^ Input for lexer.
  -> IO ()
lexTest' = parseTest' program

--------------------------------------------------------------------------------
-- Parser productions

-- | Parse whole program as a list of tokens.
program :: Parser [Token]
program = sc *> many anyTok <* eof

-- | Parse any valid token.
anyTok :: Parser Token
anyTok = literal <|> ident <|> anyKeyword <|> anyOperator

-- | Parse token of given type.
tok :: TokenType -> Parser Token
tok TIdent        = ident
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
tok TKwFail       = tokKw TKwFail
tok TKwFun        = tokKw TKwFun
tok TKwIf         = tokKw TKwIf
tok TKwImpl       = tokKw TKwImpl
tok TKwImport     = tokKw TKwImport
tok TKwIn         = tokKw TKwIn
tok TKwIs         = tokKw TKwIs
tok TKwLoop       = tokKw TKwLoop
tok TKwLet        = tokKw TKwLet
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
tok TOpSub        = tokOp TOpSub
tok TOpMul        = tokOp TOpMul
tok TOpDiv        = tokOp TOpDiv
tok TOpLParen     = tokOp TOpLParen
tok TOpRParen     = tokOp TOpRParen
tok TOpLBracket   = tokOp TOpLBracket
tok TOpRBracket   = tokOp TOpRBracket
tok TOpLBrace     = tokOp TOpLBrace
tok TOpRBrace     = tokOp TOpRBrace
tok TOpComma      = tokOp TOpComma
tok TOpColon      = tokOp TOpColon
tok TOpSemicolon  = tokOp TOpSemicolon
tok TOpEq         = tokOp TOpEq
tok TOpEqEq       = tokOp TOpEqEq
tok TOpEqEqEq     = tokOp TOpEqEqEq
tok TOpLt         = tokOp TOpLt
tok TOpLtEq       = tokOp TOpLtEq
tok TOpGt         = tokOp TOpGt
tok TOpGtEq       = tokOp TOpGtEq
tok TOpNeq        = tokOp TOpNeq
tok TOpNeqEq      = tokOp TOpNeqEq
tok TOpColonEq    = tokOp TOpColonEq
tok TOpLtSub      = tokOp TOpLtSub
tok TOpDollar     = tokOp TOpDollar
tok TOpPercent    = tokOp TOpPercent
tok TInteger      = integer
tok TFloat        = float
tok TNone         = none
tok TSucc         = succ
tok TFail         = fail
tok TString       = string'
tok TRawString    = rawstring
tok TRegexString  = regexstring

--------------------------------------------------------------------------------
-- Token productions

-- | Parse an identifier.
ident :: Parser Token
ident = (try . lexeme $ (p >>= nonReserved)) >>= mkt TIdent <?> "identifier"
 where
  p :: Parser Text
  p = identStart >:> many identContinue

  nonReserved :: Text -> Parser Text
  nonReserved w | isKeyword w = P.fail $ "Illegal identifier: " ++ toS w
                | otherwise   = return w

  isKeyword :: Text -> Bool
  isKeyword = (`BM.member` keywords)

-- | Parse any valid keyword.
anyKeyword :: Parser Token
anyKeyword = anyReserved keywords <?> "keyword"

-- | Parse any valid operator.
anyOperator :: Parser Token
anyOperator = anyReserved operators <?> "operator"

-- | Parse any valid literal.
literal :: Parser Token
literal = try none 
  <|> try succ 
  <|> try fail 
  <|> try float 
  <|> try integer 
  <|> try anyString

-- | Parse none literal
none :: Parser Token
none = lexeme n >>= mkt TNone <?> "none literal"
  where 
    n = string "none"

-- | Parse succ literal
succ :: Parser Token
succ = lexeme s >>= mkt TSucc <?> "succ literal"
  where 
    s = string "succ"

-- | Parse fail literal
fail :: Parser Token
fail = lexeme f >>= mkt TFail <?> "fail literal"
  where 
    f = string "fail"

-- | Parse any valid integer literal.
integer :: Parser Token
integer = lexeme p >>= mkt TInteger <?> "integer literal"
 where
  p           = try binary <|> try octal <|> try hexadecimal <|> try decimalNum

  binary      = char '0' >:> oneOf ['b', 'B'] >:> binaryNum
  octal       = char '0' >:> oneOf ['o', 'O'] >:> octalNum
  hexadecimal = char '0' >:> oneOf ['x', 'X'] >:> hexadecimalNum

-- | Parse any valid floating-point literal.
float :: Parser Token
float = lexeme p >>= mkt TFloat <?> "floating-point literal"
 where
  p = try (decimalNum <~> string "." <~> decimalNum <~> option "" exponent)
    <|> try (decimalNum <~> exponent)

decimalNum :: Parser Text
decimalNum = toS <$> p <?> "decimal digits"
  where p = digitChar >:> many (digitChar <|> char '_')

binaryNum :: Parser Text
binaryNum = toS <$> p <?> "binary digits"
 where
  p            = binDigitChar >:> many (binDigitChar <|> char '_')
  binDigitChar = oneOf ['0', '1'] <?> "binary digit"

octalNum :: Parser Text
octalNum = toS <$> p <?> "octal digits"
  where p = octDigitChar >:> many (octDigitChar <|> char '_')

hexadecimalNum :: Parser Text
hexadecimalNum = toS <$> p <?> "hexadecimal digits"
  where p = hexDigitChar >:> many (hexDigitChar <|> char '_')

exponent :: Parser Text
exponent = do
  e           <- T.singleton <$> oneOf ['e', 'E']
  sign        <- option "" $ T.singleton <$> oneOf ['+', '-']
  underscores <- toS <$> many (char '_')
  val         <- decimalNum
  return $ e <> sign <> underscores <> val

-- | Parse any valid string literal.
anyString :: Parser Token
anyString =
  try string' <|> try regexstring <|> try rawstring

-- | Parse valid regular string literal.
string' :: Parser Token
string' = lexeme (stringprefix <~> istring') >>= mkt TString

-- | Parse valid regex literal.
regexstring :: Parser Token
regexstring = lexeme (stringprefix <~> iregexstring) >>= mkt TRegexString

-- | Parse valid raw string literal.
rawstring :: Parser Token
rawstring = lexeme (stringprefix <~> irawstring) >>= mkt TRawString

istring' :: Parser Text
istring' =
  string "\""
    <~> (T.concat <$> many strchr)
    <~> string "\""
    <?> "regular string"

iregexstring :: Parser Text
iregexstring =
  string "x" <~> (try istring' <|> try irawstring) <?> "regex string"

irawstring :: Parser Text
irawstring = char 'r' >:> rawstring' 0 <?> "raw string"
 where
  rawstring' :: Int -> Parser Text
  rawstring' n =
    (string "\"" <~> rawstring'' n <~> string "\"")
      <|> (string "#" <~> rawstring' (n + 1) <~> string "#")

  rawstring'' :: Int -> Parser Text
  rawstring'' n = toS <$> many rwsany
   where
    rwsany = try (notChar '"') <|> try (char '"' <* notFollowedBy hashes)
    hashes = count n $ char '#'

stringprefix :: Parser Text
stringprefix = option "" stringmod

stringmod :: Parser Text
stringmod = toS <$> some (satisfy isStringModChar) <?> "string modifier"
  where isStringModChar c = c /= 'r' && c /= 'x' && isLower c

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
-- Utilities

mkt :: TokenType -> Text -> Parser Token
mkt _ty _text = return Token {..}

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
tokKw = tokReserved keywords

tokOp :: TokenType -> Parser Token
tokOp = tokReserved operators

tokReserved :: BM.Bimap Text TokenType -> TokenType -> Parser Token
tokReserved m t = symbol s >>= mkt t where s = m BM.!> t

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
  , ("fail"    , TKwFail)
  , ("fun"     , TKwFun)
  , ("if"      , TKwIf)
  , ("impl"    , TKwImpl)
  , ("import"  , TKwImport)
  , ("in"      , TKwIn)
  , ("is"      , TKwIs)
  , ("let"     , TKwLet)
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
  , ("===", TOpEqEqEq)
  , ("<"  , TOpLt)
  , ("<=" , TOpLtEq)
  , (">"  , TOpGt)
  , (">=" , TOpGtEq)
  , ("!=" , TOpNeq)
  , ("!==", TOpNeqEq)
  , (":=" , TOpColonEq)
  , ("<-" , TOpLtSub)
  , ("$"  , TOpDollar)
  , ("%"  , TOpPercent)
  ]

infixl 6 <:<
(<:<) :: (MonadParsec e s m, StringConv t Text) => m t -> m Char -> m Text
(<:<) l r = snoc <$> (toS <$> l) <*> r

infixr 6 >:>
(>:>) :: (MonadParsec e s m, StringConv t Text) => m Char -> m t -> m Text
(>:>) l r = cons <$> l <*> (toS <$> r)

infixr 6 <~>
(<~>) :: (Monoid a, MonadParsec e s m) => m a -> m a -> m a
(<~>) l r = (<>) <$> l <*> r
