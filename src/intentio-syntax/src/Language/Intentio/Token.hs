module Language.Intentio.Token where

import           Intentio.Prelude

newtype StringMod = StringMod Text
  deriving (Eq, Ord, Show)

data Token
  = Ident Text

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

  | Integer Text
  | Float Text

  | String Text StringMod
  | CharString Text StringMod
  | RawString Text StringMod
  | RegexString Text StringMod

  deriving (Eq, Show)
