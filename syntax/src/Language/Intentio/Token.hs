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
  | TKwEval
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
  | TKwNone
  | TKwNot
  | TKwOr
  | TKwReturn
  | TKwStruct
  | TKwSucc
  | TKwTrait
  | TKwType
  | TKwUnderscore
  | TKwWhere
  | TKwWhile
  | TKwXor
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

  | TOpEq
  | TOpEqEq
  | TOpEqEqEq
  | TOpGt
  | TOpGtEq
  | TOpLt
  | TOpLtEq
  | TOpNeq
  | TOpNeqEq

  | TOpColonEq
  | TOpLtSub
  | TOpDollar
  | TOpPercent

  | TInteger
  | TFloat
  | TSpecial

  | TString
  | TCharString
  | TRawString
  | TRegexString

  deriving (Eq, Ord, Show, Generic)

instance ToJSON TokenType
instance FromJSON TokenType

data Token = Token {
    _ty :: TokenType,
    _text :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Token
instance FromJSON Token

makeLenses ''Token
