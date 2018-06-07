module Language.Intentio.Token where

import           Intentio.Prelude

--------------------------------------------------------------------------------
-- Token data structures

data TokenType
  = TIdent

  | TKwAbstract
  | TKwAnd
  | TKwBreak
  | TKwCase
  | TKwConst
  | TKwContinue
  | TKwDo
  | TKwElse
  | TKwEnum
  | TKwExport
  | TKwFail
  | TKwFun
  | TKwIf
  | TKwImpl
  | TKwImport
  | TKwIn
  | TKwIs
  | TKwLet
  | TKwLoop
  | TKwModule
  | TKwNot
  | TKwOr
  | TKwReturn
  | TKwStruct
  | TKwType
  | TKwUnderscore
  | TKwWhere
  | TKwWhile
  | TKwYield

  | TOpAdd
  | TOpSub
  | TOpMul
  | TOpDiv

  | TOpLParen
  | TOpRParen
  | TOpLBracket
  | TOpRBracket
  | TOpLBrace
  | TOpRBrace
  | TOpComma
  | TOpColon
  | TOpSemicolon

  | TOpEqEq
  | TOpLt
  | TOpLtEq
  | TOpGt
  | TOpGtEq

  | TOpColonEq
  | TOpLtSub
  | TOpDollar
  | TOpPercent

  | TInteger
  | TFloat

  | TString
  | TCharString
  | TRawString
  | TRegexString

  deriving (Eq, Ord, Show)

data Token = Token {
    _ty :: TokenType,
    _text :: Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''Token
