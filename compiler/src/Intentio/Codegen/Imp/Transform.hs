module Intentio.Codegen.Imp.Transform
  ( impTransform
  )
where

import           Intentio.Prelude

import qualified Intentio.Codegen.Imp.Model    as Imp
import           Intentio.Compiler              ( CompilePure )
import qualified Intentio.Hir                  as Hir

impTransform :: Hir.Body () -> CompilePure (Imp.Body ())
impTransform = undefined
