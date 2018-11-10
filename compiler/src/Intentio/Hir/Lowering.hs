module Intentio.Hir.Lowering
  ( lowerAssembly
  , lowerModule
  )
where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM

import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , itemName
                                                , mapModulesM
                                                , moduleItems
                                                , moduleName
                                                , pushIceFor
                                                )
import           Intentio.Diagnostics           ( sourcePos )
import qualified Intentio.Hir.Model            as H
import           Intentio.Resolver              ( RS
                                                , Resolution(..)
                                                , _ResolvedItem
                                                , resolution
                                                )
import           Intentio.Util.NodeId           ( nodeId )
import           Language.Intentio.AST          ( ModuleName(..)
                                                , ItemName(..)
                                                )
import qualified Language.Intentio.AST         as A

--------------------------------------------------------------------------------
-- Lowering monad

data LowerState = LowerState
  { _currentModule :: A.Module RS
  , _freeBodyIds :: [H.BodyId]
  , _allocatedBodyIds :: HashMap ItemName H.BodyId
  }

type LowerM a = StateT LowerState CompilePure a

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''LowerState

--------------------------------------------------------------------------------
-- Lowering monad utilities

emptyLowerState :: A.Module RS -> LowerState
emptyLowerState _currentModule =
  let _freeBodyIds      = [H.BodyId 0 ..]
      _allocatedBodyIds = mempty
  in  LowerState { .. }

allocBodyId :: ItemName -> LowerM H.BodyId
allocBodyId iName = do
  (bid : bids) <- use freeBodyIds
  freeBodyIds .= bids
  allocatedBodyIds . at iName .= Just bid
  return bid

lookupBodyId :: A.ItemDecl RS -> LowerM H.BodyId
lookupBodyId item = case item ^. itemName of
  Nothing    -> lift $ pushIceFor item "Unnamed items do not have bodies."
  Just iName -> use (allocatedBodyIds . at iName) >>= \case
    Just bid -> return bid
    Nothing  -> lift $ pushIceFor item "Item has not allocated body id."

--------------------------------------------------------------------------------
-- HIR Lowering code

lowerAssembly :: Assembly (A.Module RS) -> CompilePure (Assembly (H.Module ()))
lowerAssembly = mapModulesM lowerModule

lowerModule :: A.Module RS -> CompilePure (H.Module ())
lowerModule = evalStateT lowerModule' . emptyLowerState

lowerModule' :: LowerM (H.Module ())
lowerModule' = do
  let _moduleAnn = ()
  _moduleSourcePos                <- use $ currentModule . sourcePos
  _moduleName                     <- use $ currentModule . moduleName
  _moduleExports                  <- lowerModuleExport
  _moduleImports                  <- collectImports
  (_moduleItemIds, _moduleItems ) <- lowerItems
  (_moduleBodyIds, _moduleBodies) <- lowerBodies
  let _moduleItemsNames = getItemsNames _moduleItems
  return H.Module { .. }
 where
  getItemsNames = fromList . concatMap getItemsNamesGo . toList
  getItemsNamesGo it = case it ^. itemName of
    Just n  -> [(n, it ^. H.itemId)]
    Nothing -> []

lowerModuleExport :: LowerM (HashSet ItemName)
lowerModuleExport = uses (currentModule . A.moduleExport) $ \case
  Nothing   -> mempty
  Just decl -> fromList . fmap resolvedItemName $ decl ^. A.exportDeclItems
  where resolvedItemName sid = sid ^. resolution ^?! _ResolvedItem ^. _2

collectImports :: LowerM (HashSet (ModuleName, ItemName))
collectImports = do
  cm <- use $ currentModule . moduleName
  let visit (ResolvedItem m i) | m == cm   = mempty
                               | otherwise = fromList [(m, i)]
      visit _ = mempty
  foldMapOf resolution visit <$> use currentModule

lowerItems :: LowerM ([H.ItemId], IM.IntMap (H.Item ()))
lowerItems = do
  aItems <- use $ currentModule . moduleItems
  hItems <- concat <$> mapM lowerItem aItems
  return
    ( view H.itemId <$> hItems
    , fromList . fmap (\i -> (i ^. H.itemId . H.unItemId, i)) $ hItems
    )

lowerItem :: A.ItemDecl RS -> LowerM [H.Item ()]
lowerItem item = case item ^. A.itemDeclKind of
  A.ImportItemDecl _ -> return [] -- Handled by 'collectImports'
  A.FunItemDecl    d -> pure <$> lowerFunDecl d

lowerFunDecl :: A.FunDecl RS -> LowerM (H.Item ())
lowerFunDecl af = do
  let iName = ItemName $ af ^. A.funDeclName . A.unScopeId
  bid <- allocBodyId iName
  let _itemAnn       = ()
  let _itemSourcePos = af ^. sourcePos
  let _itemId        = convert $ af ^. nodeId
  let _itemName      = Just iName
  let _itemKind      = H.FnItem bid
  return H.Item { .. }

lowerBodies :: LowerM ([H.BodyId], IM.IntMap (H.Body ()))
lowerBodies = undefined
