module Intentio.Hir.Lowering
  ( lowerAssembly
  , lowerModule
  )
where

import           Intentio.Prelude

import qualified Data.HashSet                  as HS

import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , mapModulesM
                                                )
import qualified Intentio.Hir.Model            as H
import           Intentio.Resolver              ( RS
                                                , Resolution(..)
                                                , _ResolvedItem
                                                , resolution
                                                )
import           Language.Intentio.AST          ( ModuleName
                                                , ItemName
                                                )
import qualified Language.Intentio.AST         as A

type LowerM a = ReaderT (A.Module RS) CompilePure a

lowerAssembly :: Assembly (A.Module RS) -> CompilePure (Assembly (H.Module ()))
lowerAssembly = mapModulesM lowerModule

lowerModule :: A.Module RS -> CompilePure (H.Module ())
lowerModule = runReaderT lowerModule'

lowerModule' :: LowerM (H.Module ())
lowerModule' = do
  let _moduleAnn = ()
  _moduleSourcePos <- view A.moduleSourcePos
  _moduleName      <- view A.moduleName
  _moduleExports   <- lowerModuleExport
  _moduleImports   <- collectImports
  return H.Module { .. }

lowerModuleExport :: LowerM (HashSet ItemName)
lowerModuleExport = view A.moduleExport <&> \case
  Nothing   -> mempty
  Just decl -> fromList . fmap resolvedItemName $ decl ^. A.exportDeclItems
  where resolvedItemName sid = sid ^. resolution ^?! _ResolvedItem ^. _2

collectImports :: LowerM (HashSet (ModuleName, ItemName))
collectImports = do
  cm <- view A.moduleName
  let visit (ResolvedItem m i) | m == cm   = mempty
                               | otherwise = HS.singleton (m, i)
      visit _ = mempty
  foldMapOf resolution visit <$> ask
