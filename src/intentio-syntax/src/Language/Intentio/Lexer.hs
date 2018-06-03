module Language.Intentio.Lexer
  ( Lexer
  , program
  , lex
  , lexTest
  , lexTest'
  )
where

import           Intentio.Prelude        hiding ( many
                                                , option
                                                , some
                                                , try
                                                )

import           Data.Char                      ( isLower )
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T

import           Text.Megaparsec                ( MonadParsec
                                                , Parsec
                                                , ParseError
                                                , Token
                                                , (<?>)
                                                , choice
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
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , char'
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

import qualified Language.Intentio.Token       as I

--------------------------------------------------------------------------------
-- Data types

-- | The lexer monad.
type Lexer = Parsec Void Text

--------------------------------------------------------------------------------
-- Lexer entry points

-- | Run lexer over input text. Returns either lexer error or list of tokens.
lex
  :: String -- ^ Name of source file.
  -> Text   -- ^ Input for lexer.
  -> Either (ParseError (Token Text) Void) [I.Token]
lex = parse program

-- | Run lexer over input text and print the results to stdout.
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

program :: Lexer [I.Token]
program = sc *> many itoken <* eof

itoken :: Lexer I.Token
itoken = ident
-- itoken = keyword <|> literal <|> operator <|> ident

--------------------------------------------------------------------------------
-- Token productions

ident :: Lexer I.Token
ident = I.Ident <$> (lexeme . try $ (p >>= nonReserved)) <?> "identifier"
 where
  p :: Lexer Text
  p = toS <$> ((:) <$> identStart <*> many identContinue)

  nonReserved :: Text -> Lexer Text
  nonReserved w | w `M.member` keywords = fail $ "Illegal identifier: " ++ toS w
                | otherwise             = return w

keyword :: Lexer I.Token
keyword = reserved keywords <?> "keyword"

operator :: Lexer I.Token
operator = reserved operators <?> "operator"

literal :: Lexer I.Token
literal = lexeme (choiceTry [iinteger, ifloat, istring]) <?> "literal"
 where
  iinteger :: Lexer I.Token
  iinteger =
    I.Integer
      <$> choiceTry [ibinary, ioctal, ihexadecimal, idecimal]
      <?> "integer literal"

  ifloat :: Lexer I.Token
  ifloat =
    I.Float
      <$> choiceTry
            [ concatP [idecimal, string ".", idecimal, option "" iexponent]
            , concatP [idecimal, iexponent]
            ]
      <?> "floating-point literal"

  binDigitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
  binDigitChar = oneOf ['0', '1'] <?> "binary digit"

  idecimal :: Lexer Text
  idecimal =
    toS
      <$> ((:) <$> digitChar <*> many (digitChar <|> char '_'))
      <?> "decimal digits"

  ibinary :: Lexer Text
  ibinary =
    toS
      <$> ((:) <$> binDigitChar <*> many (binDigitChar <|> char '_'))
      <?> "binary digits"

  ioctal :: Lexer Text
  ioctal =
    toS
      <$> ((:) <$> octDigitChar <*> many (octDigitChar <|> char '_'))
      <?> "octal digits"

  ihexadecimal :: Lexer Text
  ihexadecimal =
    toS
      <$> ((:) <$> hexDigitChar <*> many (hexDigitChar <|> char '_'))
      <?> "hexadecimal digits"

  iexponent :: Lexer Text
  iexponent = do
    e           <- T.singleton <$> char' 'e'
    sign        <- T.singleton <$> oneOf ("+-" :: String)
    underscores <- toS <$> many (char '_')
    val         <- idecimal
    return $ e <> sign <> underscores <> val

  istring :: Lexer I.Token
  istring = do
    modstr <- I.StringMod <$> stringmod
    cst    <- choiceTry
      [ I.String <$> istring'
      , I.CharString <$> icharstring
      , I.RegexString <$> iregexstring
      , I.RawString <$> irawstring
      ]
    return $ cst modstr

  stringmod :: Lexer Text
  stringmod = toS <$> some (satisfy isStringModChar) <?> "string modifier"
    where isStringModChar c = c /= 'c' && c /= 'r' && c /= 'x' && isLower c

  istring' :: Lexer Text
  istring' = go <?> "regular string"
   where
    go = do
      lquot <- string "\""
      chars <- many strchr
      rquot <- string "\""
      return $ lquot <> T.concat chars <> rquot

  icharstring :: Lexer Text
  icharstring = concatP [string "c\"", strchr, string "\""] <?> "char string"

  iregexstring :: Lexer Text
  iregexstring =
    (<>) <$> string "x" <*> choiceTry [istring', irawstring] <?> "regex string"

  irawstring :: Lexer Text
  irawstring = cons <$> char 'r' <*> rawstring' 0 <?> "raw string"
   where
    rawstring' :: Int -> Lexer Text
    rawstring' n = concatP [string "\"", rawstring'' n, string "\""]
      <|> concatP [string "#", rawstring' n, string "#"]

    rawstring'' :: Int -> Lexer Text
    rawstring'' n = toS <$> many rwsany
     where
      rwsany = notChar '"' <|> (char '"' <* notFollowedBy hashes)
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
      code  <- oneOf ("'\"nrt\\0" :: String)
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

reserved :: M.HashMap Text I.Token -> Lexer I.Token
reserved = try . M.foldrWithKey (\s t p -> (symbol s $> t) <|> p) empty

keywords :: M.HashMap Text I.Token
keywords = M.fromList
  [ ("abstract", I.KwAbstract)
  , ("and"     , I.KwAnd)
  , ("break"   , I.KwBreak)
  , ("case"    , I.KwCase)
  , ("const"   , I.KwConst)
  , ("continue", I.KwContinue)
  , ("do"      , I.KwDo)
  , ("else"    , I.KwElse)
  , ("enum"    , I.KwEnum)
  , ("export"  , I.KwExport)
  , ("fail"    , I.KwFail)
  , ("fun"     , I.KwFun)
  , ("if"      , I.KwIf)
  , ("impl"    , I.KwImpl)
  , ("import"  , I.KwImport)
  , ("in"      , I.KwIn)
  , ("is"      , I.KwIs)
  , ("loop"    , I.KwLoop)
  , ("module"  , I.KwModule)
  , ("not"     , I.KwNot)
  , ("or"      , I.KwOr)
  , ("return"  , I.KwReturn)
  , ("struct"  , I.KwStruct)
  , ("type"    , I.KwType)
  , ("where"   , I.KwWhere)
  , ("while"   , I.KwWhile)
  , ("yield"   , I.KwYield)
  , ("_"       , I.KwUnderscore)
  ]

operators :: M.HashMap Text I.Token
operators = M.fromList
  [ ("+" , I.OpAdd)
  , ("-" , I.OpSub)
  , ("*" , I.OpMul)
  , ("/" , I.OpDiv)
  , ("(" , I.OpLParen)
  , (")" , I.OpRParen)
  , ("[" , I.OpLBracket)
  , ("]" , I.OpRBracket)
  , ("{" , I.OpLBrace)
  , ("}" , I.OpRBrace)
  , (":" , I.OpColon)
  , (";" , I.OpSemicolon)
  , ("==", I.OpEqEq)
  , ("<" , I.OpLt)
  , ("<=", I.OpLtEq)
  , (">" , I.OpGt)
  , (">=", I.OpGtEq)
  , (":=", I.OpColonEq)
  , ("<-", I.OpLtSub)
  , ("$" , I.OpDollar)
  , ("%" , I.OpPercent)
  ]

concatP :: (Monoid a, Foldable f, MonadParsec e s m) => f (m a) -> m a
concatP = foldrM (\p rs -> (<> rs) <$> p) mempty

choiceTry :: (Functor f, Foldable f, MonadParsec e s m) => f (m a) -> m a
choiceTry = choice . map try
