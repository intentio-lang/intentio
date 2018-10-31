module Intentio.Hir.Lowering
  ( lowerAssembly
  )
where

import           Intentio.Prelude

import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                )
import qualified Intentio.Hir.Model            as H
import           Intentio.Resolver              ( RS )
import qualified Language.Intentio.AST         as A

lowerAssembly :: Assembly (A.Module RS) -> CompilePure (Assembly (H.Module ()))
lowerAssembly = undefined
