module Language.Intentio.Token
  ( TokenType(..)
  , Token(..)
  , ty
  , text
  )
where

import           Intentio.Prelude

--------------------------------------------------------------------------------
-- Token data structures

data TokenType
  = TFloat
  | TIdent
  | TInteger
  | TKwAbstract
  | TKwAnd
  | TKwAs
  | TKwBreak
  | TKwCase
  | TKwConst
  | TKwContinue
  | TKwDo
  | TKwElse
  | TKwEnum
  | TKwEval
  | TKwExport
  | TKwFun
  | TKwFail
  | TKwIf
  | TKwImpl
  | TKwImport
  | TKwIn
  | TKwIs
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
  | TOpColon
  | TOpColonEq
  | TOpComma
  | TOpDiv
  | TOpDollar
  | TOpEq
  | TOpEqEq
  | TOpGt
  | TOpGtEq
  | TOpLBrace
  | TOpLBracket
  | TOpLParen
  | TOpLt
  | TOpLtEq
  | TOpLtSub
  | TOpMul
  | TOpNeq
  | TOpPercent
  | TOpRBrace
  | TOpRBracket
  | TOpRParen
  | TOpSemicolon
  | TOpSEq
  | TOpSNeq
  | TOpSub
  | TRawString
  | TRegexString
  | TString
  deriving (Eq, Ord, Show, Generic)

instance ToJSON TokenType
instance FromJSON TokenType

data Token = Token
  { _ty   :: TokenType
  , _text :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Token
instance FromJSON Token

makeLenses ''Token
