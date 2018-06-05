module Language.Intentio.Token where

import           Intentio.Prelude

import           Language.Intentio.Debug        ( SyntaxDebugPrint(..) )

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
  | KwLet
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
