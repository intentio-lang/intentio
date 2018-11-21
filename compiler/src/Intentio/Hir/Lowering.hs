module Intentio.Hir.Lowering
  ( lowerAssembly
  , lowerModule
  )
where

import           Intentio.Prelude

import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                     ( maybe )

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
                                                , _ResolvedLocal
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
  foldMap (visit . view resolution) <$> use currentModule

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
  A.ImportItemDecl    _ -> return [] -- Handled by 'collectImports'
  A.FunItemDecl       d -> pure <$> lowerFunDecl d
  A.ExternFunItemDecl d -> pure <$> lowerExternFunDecl d

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

lowerExternFunDecl :: A.ExternFunDecl RS -> LowerM (H.Item ())
lowerExternFunDecl af = do
  let iName    = ItemName $ af ^. A.externFunDeclName . A.unScopeId
  let callConv = af ^. A.externFunDeclCallConv
  bid <- allocBodyId iName
  let _itemAnn       = ()
  let _itemSourcePos = af ^. sourcePos
  let _itemId        = convert $ af ^. nodeId
  let _itemName      = Just iName
  let _itemKind      = H.ExternFnItem callConv bid
  return H.Item { .. }

lowerBodies :: LowerM ([H.BodyId], IM.IntMap (H.Body ()))
lowerBodies = do
  aItems  <- use $ currentModule . moduleItems
  hBodies <- concat <$> mapM lowerBody aItems
  return
    ( view H.bodyId <$> hBodies
    , fromList . fmap (\i -> (i ^. H.bodyId . H.unBodyId, i)) $ hBodies
    )

lowerBody :: A.ItemDecl RS -> LowerM [H.Body ()]
lowerBody item = case item ^. A.itemDeclKind of
  A.ImportItemDecl    _ -> return []
  A.FunItemDecl       d -> pure <$> lowerFunDeclBody item d
  A.ExternFunItemDecl d -> pure <$> lowerExternFunDeclBody item d

lowerFunDeclBody :: A.ItemDecl RS -> A.FunDecl RS -> LowerM (H.Body ())
lowerFunDeclBody aItem af = do
  let _bodyAnn = ()

  _bodyId <- lookupBodyId aItem

  let (varIdsP, varsP, _bodyParams) = lowerParams $ af ^. A.funDeclParams

  (varIdsB, varsB, _bodyValue) <- runLowerBody $ af ^. A.funDeclBody

  let _bodyVarIds = varIdsP <> varIdsB
  let _bodyVars   = varsP <> varsB

  return H.Body { .. }

lowerExternFunDeclBody
  :: A.ItemDecl RS -> A.ExternFunDecl RS -> LowerM (H.Body ())
lowerExternFunDeclBody aItem af = do
  let _bodyAnn = ()
  _bodyId <- lookupBodyId aItem
  let (_bodyVarIds, _bodyVars, _bodyParams) =
        lowerParams $ af ^. A.externFunDeclParams
  let emptyBlock = H.Block () (() ^. sourcePos) []
  let _bodyValue = H.Expr () (() ^. sourcePos) $ H.BlockExpr emptyBlock
  return H.Body { .. }

lowerParams
  :: [A.FunParam RS] -> ([H.VarId], IM.IntMap (H.Var ()), [H.Param ()])
lowerParams aps =
  ( view H.varId <$> vars
  , fromList . fmap (\i -> (i ^. H.varId . H.unVarId, i)) $ vars
  , pars
  )
 where
  vars = concatMapOf (each . A.funParamId) lowerDeclaringScopeIdToVar aps
  pars =
    H.Param
      .   convert
      .   (^?! _ResolvedLocal)
      .   view (A.funParamId . resolution)
      <$> aps

runLowerBody
  :: A.FunBody RS -> LowerM ([H.VarId], IM.IntMap (H.Var ()), H.Expr ())
runLowerBody afb = do
  let vars = varsBlock $ afb ^. A.funBodyBlock
  value <- lowerBlockToExpr $ afb ^. A.funBodyBlock
  return
    ( view H.varId <$> vars
    , fromList . fmap (\i -> (i ^. H.varId . H.unVarId, i)) $ vars
    , value
    )
 where
  varsBlock = concatMap varsStmt . view A.blockStmts

  varsStmt ast = case ast ^. A.stmtKind of
    A.AssignStmt s e -> lowerDeclaringScopeIdToVar s <> varsExpr e
    A.ExprStmt e     -> varsExpr e

  varsExpr aie = case aie ^. A.exprKind of
    A.IdExpr    (A.ScopeId' s) -> lowerDeclaringScopeIdToVar s
    A.BlockExpr b              -> varsBlock b
    A.SuccExpr  e              -> varsExpr e
    A.FailExpr  e              -> varsExpr e
    A.UnExpr _ e               -> varsExpr e
    A.BinExpr _ l r            -> varsExpr l <> varsExpr r
    A.CallExpr  c a            -> concatMap varsExpr (c : a)
    A.WhileExpr e b            -> varsExpr e <> varsBlock b
    A.IfExpr c i e -> varsExpr c <> varsBlock i <> maybe [] varsBlock e
    A.ParenExpr  e             -> varsExpr e
    A.ReturnExpr e             -> maybe [] varsExpr e
    _                          -> []

lowerDeclaringScopeIdToVar :: A.ScopeId RS -> [H.Var ()]
lowerDeclaringScopeIdToVar s
  | s ^. resolution == ResolvedLocal (s ^. nodeId) = [lowerScopeIdToVar s]
  | otherwise = []

lowerScopeIdToVar :: A.ScopeId RS -> H.Var ()
lowerScopeIdToVar sid =
  let _varAnn       = ()
      _varSourcePos = sid ^. sourcePos
      _varId        = lowerScopeIdToVarId sid
      _varName      = sid ^. A.unScopeId
  in  H.Var { .. }

lowerScopeIdToVarId :: A.ScopeId RS -> H.VarId
lowerScopeIdToVarId = convert . (^?! _ResolvedLocal) . view resolution

lowerBlockToExpr :: A.Block RS -> LowerM (H.Expr ())
lowerBlockToExpr ab = do
  hb <- lowerBlock ab
  let _exprAnn       = ()
  let _exprSourcePos = ab ^. sourcePos
  let _exprKind      = H.BlockExpr hb
  return H.Expr { .. }

lowerBlock :: A.Block RS -> LowerM (H.Block ())
lowerBlock ab = do
  let _blockAnn       = ()
  let _blockSourcePos = ab ^. sourcePos
  _blockExprs <- mapM lowerStmt $ ab ^. A.blockStmts
  return H.Block { .. }

lowerStmt :: A.Stmt RS -> LowerM (H.Expr ())
lowerStmt ast = case ast ^. A.stmtKind of
  A.AssignStmt asid aexpr -> do
    hexpr <- lowerExpr aexpr
    let _exprAnn       = ()
    let _exprSourcePos = ast ^. sourcePos
    let _exprKind = H.AssignExpr (lowerScopeIdToVarId asid) hexpr
    return H.Expr { .. }
  A.ExprStmt aexpr -> lowerExpr aexpr

lowerExpr :: A.Expr RS -> LowerM (H.Expr ())
lowerExpr aexpr = case aexpr ^. A.exprKind of
  A.IdExpr    aid  -> mk . H.PathExpr <$> lowerAnyIdToPath aid

  A.LitExpr   alit -> return . mk . H.LitExpr . lowerLit $ alit

  A.BlockExpr ab   -> lowerBlockToExpr ab

  A.SuccExpr  aie  -> mk . H.SuccExpr <$> lowerExpr aie

  A.FailExpr  aie  -> mk . H.FailExpr <$> lowerExpr aie

  A.UnExpr aop aie -> H.UnExpr <$> pure (void aop) <*> lowerExpr aie <&> mk

  A.BinExpr aop alhs arhs ->
    H.BinExpr <$> pure (void aop) <*> lowerExpr alhs <*> lowerExpr arhs <&> mk

  A.CallExpr acallee aargs ->
    H.CallExpr <$> lowerExpr acallee <*> mapM lowerExpr aargs <&> mk

  A.WhileExpr acond ab ->
    H.WhileExpr <$> lowerExpr acond <*> lowerBlockToExpr ab <&> mk

  A.IfExpr acond athen aelse ->
    H.IfExpr
      <$> lowerExpr acond
      <*> lowerBlockToExpr athen
      <*> mapM lowerBlockToExpr aelse
      <&> mk

  A.ParenExpr  aie        -> lowerExpr aie

  A.ReturnExpr (Just aie) -> mk . H.ReturnExpr <$> lowerExpr aie

  A.ReturnExpr Nothing    -> return . mk . H.ReturnExpr $ H.Expr
    { _exprAnn       = ()
    , _exprSourcePos = () ^. sourcePos
    , _exprKind      = H.LitExpr H.Lit { _litAnn       = ()
                                       , _litSourcePos = () ^. sourcePos
                                       , _litKind      = H.NoneLit
                                       }
    }
 where
  _exprAnn       = ()
  _exprSourcePos = aexpr ^. sourcePos

  mk _exprKind = H.Expr { .. }

lowerAnyIdToPath :: A.AnyId RS -> LowerM (H.Path ())
lowerAnyIdToPath ident = do
  let _pathAnn       = ()
  let _pathSourcePos = ident ^. sourcePos
  _pathKind <- case ident ^. resolution of
    ResolvedModule _ ->
      lift $ pushIceFor ident "Cannot lower module identifier."
    ResolvedItem mName iName -> return $ H.ToItem mName iName
    ResolvedLocal nid        -> return . H.ToVar . convert $ nid
    NotApplicable            -> unreachable
  return H.Path { .. }

lowerLit :: A.Lit RS -> H.Lit ()
lowerLit alit =
  let _litAnn       = ()
      _litSourcePos = alit ^. sourcePos
      _litKind      = case alit ^. A.litKind of
        A.IntegerLit   v -> H.IntegerLit v
        A.FloatLit     v -> H.FloatLit v
        A.StringLit    v -> H.StringLit v
        A.RawStringLit v -> H.StringLit v
        A.NoneLit        -> H.NoneLit
  in  H.Lit { .. }
