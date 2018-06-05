module Language.Intentio.Token where

import           Intentio.Prelude

-- import           Data.List                      ( span )

import           Language.Intentio.Debug        ( SyntaxDebugPrint(..) )

-- import qualified Text.Megaparsec               as M

data TokenType
  = Ident

  | KwAbstract
  | KwAnd
  | KwBreak
  | KwCase
  | KwConst
  | KwContinue
  | KwDo
  | KwElse
  | KwEnum
  | KwExport
  | KwFail
  | KwFun
  | KwIf
  | KwImpl
  | KwImport
  | KwIn
  | KwIs
  | KwLoop
  | KwModule
  | KwNot
  | KwOr
  | KwReturn
  | KwStruct
  | KwType
  | KwUnderscore
  | KwWhere
  | KwWhile
  | KwYield

  | OpAdd
  | OpSub
  | OpMul
  | OpDiv

  | OpLParen
  | OpRParen
  | OpLBracket
  | OpRBracket
  | OpLBrace
  | OpRBrace
  | OpColon
  | OpSemicolon

  | OpEqEq
  | OpLt
  | OpLtEq
  | OpGt
  | OpGtEq

  | OpColonEq
  | OpLtSub
  | OpDollar
  | OpPercent

  | Integer
  | Float

  | String
  | CharString
  | RawString
  | RegexString

  deriving (Eq, Ord, Show)

data Token = Token {
    ty :: TokenType,
    text :: Text
  }
  deriving (Eq, Ord, Show)

instance SyntaxDebugPrint Token where
  syntaxDebugPrint = show


-- instance M.Stream [Token] where
--   type Token [Token] = Token
--   type Tokens [Token] = [Token]
--   tokenToChunk Proxy = pure
--   tokensToChunk Proxy = identity
--   chunkToTokens Proxy = identity
--   chunkLength Proxy = length
--   chunkEmpty Proxy = null
--   -- advance1 Proxy = defaultAdvance1
--   -- advanceN Proxy w = foldl' (defaultAdvance1 w)
--   advance1 = undefined
--   advanceN = undefined
--   take1_ [] = Nothing
--   take1_ (t:ts) = Just (t, ts)
--   takeN_ n s
--     | n <= 0 = Just ([], s)
--     | null s = Nothing
--     | otherwise = Just $ splitAt n s
--   takeWhile_ = span
