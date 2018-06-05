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
import qualified Data.HashMap.Strict           as M
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
tok KwAbstract   = undefined
tok KwAnd        = undefined
tok KwBreak      = undefined
tok KwCase       = undefined
tok KwConst      = undefined
tok KwContinue   = undefined
tok KwDo         = undefined
tok KwElse       = undefined
tok KwEnum       = undefined
tok KwExport     = undefined
tok KwFail       = undefined
tok KwFun        = undefined
tok KwIf         = undefined
tok KwImpl       = undefined
tok KwImport     = undefined
tok KwIn         = undefined
tok KwIs         = undefined
tok KwLet        = undefined
tok KwLoop       = undefined
tok KwModule     = undefined
tok KwNot        = undefined
tok KwOr         = undefined
tok KwReturn     = undefined
tok KwStruct     = undefined
tok KwType       = undefined
tok KwUnderscore = undefined
tok KwWhere      = undefined
tok KwWhile      = undefined
tok KwYield      = undefined
tok OpAdd        = undefined
tok OpSub        = undefined
tok OpMul        = undefined
tok OpDiv        = undefined
tok OpLParen     = undefined
tok OpRParen     = undefined
tok OpLBracket   = undefined
tok OpRBracket   = undefined
tok OpLBrace     = undefined
tok OpRBrace     = undefined
tok OpColon      = undefined
tok OpSemicolon  = undefined
tok OpEqEq       = undefined
tok OpLt         = undefined
tok OpLtEq       = undefined
tok OpGt         = undefined
tok OpGtEq       = undefined
tok OpColonEq    = undefined
tok OpLtSub      = undefined
tok OpDollar     = undefined
tok OpPercent    = undefined
tok Integer      = integer
tok Float        = float
tok String       = undefined
tok CharString   = undefined
tok RawString    = undefined
tok RegexString  = undefined

--------------------------------------------------------------------------------
-- Token productions

ident :: Lexer Token
ident = (try . lexeme $ (p >>= nonReserved)) >>= mkt Ident <?> "identifier"
 where
  p :: Lexer Text
  p = toS <$> ((:) <$> identStart <*> many identContinue)

  nonReserved :: Text -> Lexer Text
  nonReserved w | isKeyword w = fail $ "Illegal identifier: " ++ toS w
                | otherwise   = return w

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

  binary      = char '0' <::> oneOf ['b', 'B'] <::> binaryNum
  octal       = char '0' <::> oneOf ['o', 'O'] <::> octalNum
  hexadecimal = char '0' <::> oneOf ['x', 'X'] <::> hexadecimalNum

float :: Lexer Token
float =
  try (decimalNum <~> string "." <~> decimalNum <~> option "" exponent)
    <|> try (decimalNum <~> exponent)
    >>= mkt Float
    <?> "floating-point literal"

decimalNum :: Lexer Text
decimalNum = toS <$> p <?> "decimal digits"
  where p = (:) <$> digitChar <*> many (digitChar <|> char '_')

binaryNum :: Lexer Text
binaryNum = toS <$> p <?> "binary digits"
 where
  p            = (:) <$> binDigitChar <*> many (binDigitChar <|> char '_')
  binDigitChar = oneOf ['0', '1'] <?> "binary digit"

octalNum :: Lexer Text
octalNum = toS <$> p <?> "octal digits"
  where p = (:) <$> octDigitChar <*> many (octDigitChar <|> char '_')

hexadecimalNum :: Lexer Text
hexadecimalNum = toS <$> p <?> "hexadecimal digits"
  where p = (:) <$> hexDigitChar <*> many (hexDigitChar <|> char '_')

exponent :: Lexer Text
exponent = do
  e           <- T.singleton <$> oneOf ['e', 'E']
  sign        <- option "" $ T.singleton <$> oneOf ['+', '-']
  underscores <- toS <$> many (char '_')
  val         <- decimalNum
  return $ e <> sign <> underscores <> val

anyString :: Lexer Token
anyString =
  try (prefix <~> istring' >>= mkt String)
    <|> try (prefix <~> icharstring >>= mkt CharString)
    <|> try (prefix <~> iregexstring >>= mkt RegexString)
    <|> try (prefix <~> irawstring >>= mkt RawString)
 where
  prefix = option "" stringmod

  stringmod :: Lexer Text
  stringmod = toS <$> some (satisfy isStringModChar) <?> "string modifier"
    where isStringModChar c = c /= 'c' && c /= 'r' && c /= 'x' && isLower c

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
irawstring = char 'r' <::> rawstring' 0 <?> "raw string"
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

  asciiesc = do
    prefix <- string "\\x"
    d1     <- hexDigitChar
    d2     <- hexDigitChar
    return $ prefix `snoc` d1 `snoc` d2

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

isKeyword :: Text -> Bool
isKeyword = (`M.member` keywords)

anyReserved :: M.HashMap Text TokenType -> Lexer Token
anyReserved = try . M.foldrWithKey (\s t p -> (symbol s >>= mkt t) <|> p) empty

keywords :: M.HashMap Text TokenType
keywords = M.fromList
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

operators :: M.HashMap Text TokenType
operators = M.fromList
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

infixr 6 <::>
(<::>) :: MonadParsec e s m => m Char -> m Text -> m Text
(<::>) l r = T.cons <$> l <*> r

infixr 6 <~>
(<~>) :: (Monoid a, MonadParsec e s m) => m a -> m a -> m a
(<~>) l r = (<>) <$> l <*> r
