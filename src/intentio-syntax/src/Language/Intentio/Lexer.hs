module Language.Intentio.Lexer
  ( Lexer
  , lex
  , lexTest
  , lexTest'
  , program
  , anyTok
  , tok
  , anyKeyword
  , anyOperator
  , literal
  , integer
  , float
  , anyString
  )
where

import           Intentio.Prelude        hiding ( many
                                                , option
                                                , some
                                                , try
                                                , exponent
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
-- Data types

-- | The lexer monad.
type Lexer = Parsec Void Text

type LexerError = ParseError (MP.Token Text) Void

--------------------------------------------------------------------------------
-- Lexer-only parsing entry points

-- | Run lexer over input text. Returns either lexer error or list of tokens.
lex
  :: String -- ^ Name of source file.
  -> Text   -- ^ Input for lexer.
  -> Either LexerError [Token]
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
-- Lexer productions

program :: Lexer [Token]
program = sc *> many anyTok <* eof

anyTok :: Lexer Token
anyTok = literal <|> ident <|> anyKeyword <|> anyOperator

tok :: TokenType -> Lexer Token
tok Ident        = ident
tok KwAbstract   = tokKw KwAbstract
tok KwAnd        = tokKw KwAnd
tok KwBreak      = tokKw KwBreak
tok KwCase       = tokKw KwCase
tok KwConst      = tokKw KwConst
tok KwContinue   = tokKw KwContinue
tok KwDo         = tokKw KwDo
tok KwElse       = tokKw KwElse
tok KwEnum       = tokKw KwEnum
tok KwExport     = tokKw KwExport
tok KwFail       = tokKw KwFail
tok KwFun        = tokKw KwFun
tok KwIf         = tokKw KwIf
tok KwImpl       = tokKw KwImpl
tok KwImport     = tokKw KwImport
tok KwIn         = tokKw KwIn
tok KwIs         = tokKw KwIs
tok KwLoop       = tokKw KwLoop
tok KwModule     = tokKw KwModule
tok KwNot        = tokKw KwNot
tok KwOr         = tokKw KwOr
tok KwReturn     = tokKw KwReturn
tok KwStruct     = tokKw KwStruct
tok KwType       = tokKw KwType
tok KwUnderscore = tokKw KwUnderscore
tok KwWhere      = tokKw KwWhere
tok KwWhile      = tokKw KwWhile
tok KwYield      = tokKw KwYield
tok OpAdd        = tokOp OpAdd
tok OpSub        = tokOp OpSub
tok OpMul        = tokOp OpMul
tok OpDiv        = tokOp OpDiv
tok OpLParen     = tokOp OpLParen
tok OpRParen     = tokOp OpRParen
tok OpLBracket   = tokOp OpLBracket
tok OpRBracket   = tokOp OpRBracket
tok OpLBrace     = tokOp OpLBrace
tok OpRBrace     = tokOp OpRBrace
tok OpColon      = tokOp OpColon
tok OpSemicolon  = tokOp OpSemicolon
tok OpEqEq       = tokOp OpEqEq
tok OpLt         = tokOp OpLt
tok OpLtEq       = tokOp OpLtEq
tok OpGt         = tokOp OpGt
tok OpGtEq       = tokOp OpGtEq
tok OpColonEq    = tokOp OpColonEq
tok OpLtSub      = tokOp OpLtSub
tok OpDollar     = tokOp OpDollar
tok OpPercent    = tokOp OpPercent
tok Integer      = integer
tok Float        = float
tok String       = string'
tok CharString   = charstring
tok RawString    = rawstring
tok RegexString  = regexstring

--------------------------------------------------------------------------------
-- Token productions

ident :: Lexer Token
ident = (try . lexeme $ (p >>= nonReserved)) >>= mkt Ident <?> "identifier"
 where
  p :: Lexer Text
  p = identStart >:> many identContinue

  nonReserved :: Text -> Lexer Text
  nonReserved w | isKeyword w = fail $ "Illegal identifier: " ++ toS w
                | otherwise   = return w

  isKeyword :: Text -> Bool
  isKeyword = (`BM.member` keywords)

anyKeyword :: Lexer Token
anyKeyword = anyReserved keywords <?> "keyword"

anyOperator :: Lexer Token
anyOperator = anyReserved operators <?> "operator"

literal :: Lexer Token
literal = lexeme $ try float <|> try integer <|> try anyString

integer :: Lexer Token
integer = p >>= mkt Integer <?> "integer literal"
 where
  p           = try binary <|> try octal <|> try hexadecimal <|> try decimalNum

  binary      = char '0' >:> oneOf ['b', 'B'] >:> binaryNum
  octal       = char '0' >:> oneOf ['o', 'O'] >:> octalNum
  hexadecimal = char '0' >:> oneOf ['x', 'X'] >:> hexadecimalNum

float :: Lexer Token
float =
  try (decimalNum <~> string "." <~> decimalNum <~> option "" exponent)
    <|> try (decimalNum <~> exponent)
    >>= mkt Float
    <?> "floating-point literal"

decimalNum :: Lexer Text
decimalNum = toS <$> p <?> "decimal digits"
  where p = digitChar >:> many (digitChar <|> char '_')

binaryNum :: Lexer Text
binaryNum = toS <$> p <?> "binary digits"
 where
  p            = binDigitChar >:> many (binDigitChar <|> char '_')
  binDigitChar = oneOf ['0', '1'] <?> "binary digit"

octalNum :: Lexer Text
octalNum = toS <$> p <?> "octal digits"
  where p = octDigitChar >:> many (octDigitChar <|> char '_')

hexadecimalNum :: Lexer Text
hexadecimalNum = toS <$> p <?> "hexadecimal digits"
  where p = hexDigitChar >:> many (hexDigitChar <|> char '_')

exponent :: Lexer Text
exponent = do
  e           <- T.singleton <$> oneOf ['e', 'E']
  sign        <- option "" $ T.singleton <$> oneOf ['+', '-']
  underscores <- toS <$> many (char '_')
  val         <- decimalNum
  return $ e <> sign <> underscores <> val

anyString :: Lexer Token
anyString =
  try string' <|> try charstring <|> try regexstring <|> try rawstring

string' :: Lexer Token
string' = stringprefix <~> istring' >>= mkt String

charstring :: Lexer Token
charstring = stringprefix <~> icharstring >>= mkt CharString

regexstring :: Lexer Token
regexstring = stringprefix <~> iregexstring >>= mkt RegexString

rawstring :: Lexer Token
rawstring = stringprefix <~> irawstring >>= mkt RawString

istring' :: Lexer Text
istring' =
  string "\""
    <~> (T.concat <$> many strchr)
    <~> string "\""
    <?> "regular string"

icharstring :: Lexer Text
icharstring = string "c\"" <~> strchr <~> string "\"" <?> "char string"

iregexstring :: Lexer Text
iregexstring =
  string "x" <~> (try istring' <|> try irawstring) <?> "regex string"

irawstring :: Lexer Text
irawstring = char 'r' >:> rawstring' 0 <?> "raw string"
 where
  rawstring' :: Int -> Lexer Text
  rawstring' n =
    (string "\"" <~> rawstring'' n <~> string "\"")
      <|> (string "#" <~> rawstring' (n + 1) <~> string "#")

  rawstring'' :: Int -> Lexer Text
  rawstring'' n = toS <$> many rwsany
   where
    rwsany = try (notChar '"') <|> try (char '"' <* notFollowedBy hashes)
    hashes = count n $ char '#'

stringprefix :: Lexer Text
stringprefix = option "" stringmod

stringmod :: Lexer Text
stringmod = toS <$> some (satisfy isStringModChar) <?> "string modifier"
  where isStringModChar c = c /= 'c' && c /= 'r' && c /= 'x' && isLower c

strchr :: Lexer Text
strchr =
  try escseq
    <|> (T.singleton <$> satisfy (\c -> c /= '"' && c /= '\\'))
    <?> "string character or escape sequence"

escseq :: Lexer Text
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

mkt :: TokenType -> Text -> Lexer Token
mkt t s = return $ Token {ty = t, text = s}

sc :: Lexer ()
sc = L.space space1 lineComment empty
  where lineComment = L.skipLineComment "#"

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

identStart :: Lexer Char
identStart = letterChar <|> char '_'

identContinue :: Lexer Char
identContinue = alphaNumChar <|> char '_' <|> char '\''

anyReserved :: BM.Bimap Text TokenType -> Lexer Token
anyReserved = try . BM.fold (\s t p -> (symbol s >>= mkt t) <|> p) empty

tokKw :: TokenType -> Lexer Token
tokKw = tokReserved keywords

tokOp :: TokenType -> Lexer Token
tokOp = tokReserved operators

tokReserved :: BM.Bimap Text TokenType -> TokenType -> Lexer Token
tokReserved m t = symbol s >>= mkt t
    where s = m BM.!> t

keywords :: BM.Bimap Text TokenType
keywords = BM.fromList
  [ ("abstract", KwAbstract)
  , ("and"     , KwAnd)
  , ("break"   , KwBreak)
  , ("case"    , KwCase)
  , ("const"   , KwConst)
  , ("continue", KwContinue)
  , ("do"      , KwDo)
  , ("else"    , KwElse)
  , ("enum"    , KwEnum)
  , ("export"  , KwExport)
  , ("fail"    , KwFail)
  , ("fun"     , KwFun)
  , ("if"      , KwIf)
  , ("impl"    , KwImpl)
  , ("import"  , KwImport)
  , ("in"      , KwIn)
  , ("is"      , KwIs)
  , ("loop"    , KwLoop)
  , ("module"  , KwModule)
  , ("not"     , KwNot)
  , ("or"      , KwOr)
  , ("return"  , KwReturn)
  , ("struct"  , KwStruct)
  , ("type"    , KwType)
  , ("where"   , KwWhere)
  , ("while"   , KwWhile)
  , ("yield"   , KwYield)
  , ("_"       , KwUnderscore)
  ]

operators :: BM.Bimap Text TokenType
operators = BM.fromList
  [ ("+" , OpAdd)
  , ("-" , OpSub)
  , ("*" , OpMul)
  , ("/" , OpDiv)
  , ("(" , OpLParen)
  , (")" , OpRParen)
  , ("[" , OpLBracket)
  , ("]" , OpRBracket)
  , ("{" , OpLBrace)
  , ("}" , OpRBrace)
  , (":" , OpColon)
  , (";" , OpSemicolon)
  , ("==", OpEqEq)
  , ("<" , OpLt)
  , ("<=", OpLtEq)
  , (">" , OpGt)
  , (">=", OpGtEq)
  , (":=", OpColonEq)
  , ("<-", OpLtSub)
  , ("$" , OpDollar)
  , ("%" , OpPercent)
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
