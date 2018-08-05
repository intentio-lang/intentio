module Intentio.Compiler.Context
  ( CompileCtx
  , freshCompileCtx
  , compileDiagnostics
  )
where

import           Intentio.Prelude

import           Intentio.Diagnostics           ( Diagnostic )

data CompileCtx = CompileCtx
  { _compileDiagnostics :: [Diagnostic]
  } deriving (Show, Eq)
makeLenses ''CompileCtx

freshCompileCtx :: CompileCtx
freshCompileCtx = CompileCtx {_compileDiagnostics = []}
