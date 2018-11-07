module Intentio.Hir.Lowering
  ( lowerAssembly
  )
where

import           Intentio.Prelude

import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , mapModulesM
                                                )
import qualified Intentio.Hir.Model            as H
import           Intentio.Resolver              ( RS )
import qualified Language.Intentio.AST         as A

lowerAssembly :: Assembly (A.Module RS) -> CompilePure (Assembly (H.Module ()))
lowerAssembly = mapModulesM lowerModule

lowerModule :: A.Module RS -> CompilePure (H.Module ())
lowerModule aMod = do
  let _moduleAnn = ()
  let _moduleSourcePos = aMod ^. A.moduleSourcePos
  let _moduleName = aMod ^. A.moduleName
  return H.Module {..}
