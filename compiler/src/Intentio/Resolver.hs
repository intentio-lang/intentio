{-# LANGUAGE AllowAmbiguousTypes #-}

module Intentio.Resolver
  ( Resolution(..)
  , HasResolution(..)
  , RS
  , _ResolvedItem
  , _ResolvedLocal
  , _Unresolved
  , _NotApplicable
  , resolveAssembly
  )
where

import           Intentio.Prelude

import qualified Data.HashMap.Strict           as HMS
import qualified Data.HashSet                  as HS
import qualified Data.List.NonEmpty            as NE

import           Intentio.Annotated             ( Annotated(..) )
import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , assemblyModules
                                                , forModulesM
                                                , itemName
                                                , mapModules
                                                , pushErrorFor
                                                , pushIceFor
                                                , pushWarningFor
                                                )
import           Intentio.Diagnostics           ( SourcePos
                                                , HasSourcePos
                                                , diagnosticShow
                                                , sourcePos
                                                )
import           Intentio.Util.NodeId           ( NodeId
                                                , HasNodeId
                                                , nodeId
                                                )
import qualified Intentio.Util.NodeId          as NodeId
import           Language.Intentio.AST

--------------------------------------------------------------------------------
-- Name resolution result types

data Resolution
  = ResolvedModule ModuleName
  | ResolvedItem ModuleName ItemName
  | ResolvedLocal NodeId
  | Unresolved
  | NotApplicable
  deriving (Show, Eq)

type RS = (NodeId, Resolution)

class HasResolution a where
  resolution :: Lens' a Resolution

instance HasResolution Resolution where
  resolution = id
  {-# INLINE resolution #-}

instance HasResolution RS where
  resolution = _2
  {-# INLINE resolution #-}

instance Annotated a => HasResolution (a RS) where
  resolution = ann . resolution
  {-# INLINE resolution #-}

--------------------------------------------------------------------------------
-- Scope data type

data ScopeKind
  = GlobalScope
  | ItemScope
  | VariableScope
  deriving (Show, Eq)

type ScopeName = Text

data Scope n = Scope
  { _scopeKind             :: ScopeKind
  , _scopeName             :: ScopeName
  , _scopeIds              :: HMS.HashMap n (ResolvesTo n, SourcePos)
  , _scopeWriteTransparent :: Bool
  }

deriving instance Name n => Show (Scope n)

--------------------------------------------------------------------------------
-- Name class

class (Hashable n, Show n, Eq n, Show (ResolvesTo n)) => Name n
 where
  type ResolvesTo n :: *

  showName :: n -> Text

  scopeStack :: Lens' ResolveCtx (NonEmpty (Scope n))

class Name n => ToName a n | a -> n where
  toName :: a -> n

--------------------------------------------------------------------------------
-- Value namespace

newtype ValueName = ValueName { _unValueName :: Text }
  deriving (Show, Eq, Ord, Hashable)

data ValueResolvesTo
  = ToItem ModuleName ItemName
  | ToNodeId NodeId
  deriving (Show)

instance Convertible ValueResolvesTo Resolution where
  safeConvert (ToItem m n) = Right $ ResolvedItem m n
  safeConvert (ToNodeId n) = Right $ ResolvedLocal n

instance Name ValueName where
  type ResolvesTo ValueName = ValueResolvesTo
  showName   = _unValueName
  scopeStack = lens getValueScopeStack (\c s -> c { getValueScopeStack = s })

instance ToName ValueName ValueName where
  toName = id

instance ToName Text ValueName where
  toName = ValueName

instance ToName ItemName ValueName where
  toName (ItemName n) = ValueName n

instance ToName (ScopeId a) ValueName where
  toName = toName . view unScopeId

instance ToName (Qid a) ValueName where
  toName = toName . view qidScope

--------------------------------------------------------------------------------
-- Module namespace

instance Name ModuleName where
  type ResolvesTo ModuleName = ModuleName
  showName   = _unModuleName
  scopeStack = lens getModuleScopeStack (\c s -> c { getModuleScopeStack = s })

instance ToName ModuleName ModuleName where
  toName = id

instance ToName (ModId a) ModuleName where
  toName = ModuleName . view unModId

--------------------------------------------------------------------------------
-- Name resolver state data types and monads

newtype SharedResolveCtx = SharedResolveCtx
  { _sharedGlobalExports     :: HMS.HashMap ModuleName (HashSet ItemName)
  }
  deriving (Show)

data ResolveCtx = ResolveCtx
  { _globalExports      :: HMS.HashMap ModuleName (HashSet ItemName)
  , _currentModule      :: Module NodeId
  , getValueScopeStack  :: NonEmpty (Scope ValueName)
  , getModuleScopeStack :: NonEmpty (Scope ModuleName)
  }
  deriving (Show)

type ResolveM a = StateT ResolveCtx CompilePure a

--------------------------------------------------------------------------------
-- Lenses

makePrisms ''Resolution
makeLenses ''Scope
makeLenses ''SharedResolveCtx
makeLenses ''ResolveCtx

--------------------------------------------------------------------------------
-- Functions for manipulating name resolution state

showScopeKind :: ScopeKind -> Text
showScopeKind GlobalScope   = "g"
showScopeKind ItemScope     = "i"
showScopeKind VariableScope = "v"

showScopeName :: forall n . Name n => Scope n -> Text
showScopeName s = (s ^. scopeName) <> "/" <> (s ^. scopeKind & showScopeKind)

mkScope :: forall n . Name n => ScopeKind -> ScopeName -> Scope n
mkScope _scopeKind _scopeName =
  let _scopeIds              = mempty
      _scopeWriteTransparent = False
  in  Scope { .. }

mkScopeT
  :: forall n a
   . (Name n, HasNodeId a)
  => ScopeKind
  -> ScopeName
  -> a
  -> Scope n
mkScopeT k n a = mkScope k (n <> ":" <> show (a ^. nodeId & fromEnum))

mkScopeN
  :: forall n a m . (Name n, Name m, ToName a m) => ScopeKind -> a -> Scope n
mkScopeN k a = mkScope @n k (showName $ toName a)

emptyScopeStack :: forall n . Name n => ModuleName -> NonEmpty (Scope n)
emptyScopeStack (ModuleName n) = mkScope GlobalScope n :| []

currentScope :: forall n . Name n => Lens' ResolveCtx (Scope n)
currentScope = lens getter setter
 where
  getter = NE.head . view (scopeStack @n)
  setter c s = c & scopeStack @n %~ \(_ :| ss) -> s :| ss

pushScope :: forall n . Name n => Scope n -> ResolveM ()
pushScope Scope { _scopeKind = GlobalScope } = do
  m <- use currentModule
  lift $ pushIceFor m "Cannot push global scope"
pushScope s = scopeStack @n %= NE.cons s

popScope :: forall n . Name n => ResolveM ()
popScope = uses (scopeStack @n) NE.uncons >>= \case
  (_, Just st) -> scopeStack .= st
  (_, Nothing) -> do
    m <- use currentModule
    lift $ pushIceFor m "Cannot pop global scope"

withScope :: forall n a . Name n => Scope n -> ResolveM a -> ResolveM a
withScope s f = pushScope @n s *> f <* popScope @n

lookupWithDefinitionPos
  :: forall n a
   . (Name n, ToName a n)
  => a
  -> ResolveM (Maybe (ResolvesTo n, SourcePos))
lookupWithDefinitionPos name = asum . fmap goScope <$> use scopeStack
  where goScope scope = scope ^. scopeIds . at (toName name)

lookup
  :: forall n a . (Name n, ToName a n) => a -> ResolveM (Maybe (ResolvesTo n))
lookup name = preview (_Just . _1) <$> lookupWithDefinitionPos name

redefine
  :: forall n a t
   . (Name n, ToName a n, HasSourcePos t)
  => t
  -> a
  -> ResolvesTo n
  -> ResolveM ()
redefine tg name resolvesTo = scopeStack @n %= fromList . go . toList
 where
  entry = (resolvesTo, tg ^. sourcePos)

  go []       = unreachable
  go (s@Scope { _scopeWriteTransparent = True } : ss) = s : go ss
  go (s : ss) = (s & scopeIds . at (toName name) ?~ entry) : ss

define
  :: forall n a t
   . (Name n, ToName a n, HasSourcePos t)
  => t
  -> a
  -> ResolvesTo n
  -> ResolveM ()
define tg name resolvesTo = lookupWithDefinitionPos name >>= \case
  Just (_, sp) -> lift . pushErrorFor tg $ errAlreadyDefined (toName name) sp
  Nothing      -> redefine tg name resolvesTo

lookupOrDefine
  :: forall n a t
   . (Name n, ToName a n, HasSourcePos t)
  => t
  -> a
  -> ResolvesTo n
  -> ResolveM (ResolvesTo n)
lookupOrDefine tg name resolvesTo = lookup name >>= \case
  Just r  -> return r
  Nothing -> do
    redefine tg name resolvesTo
    return resolvesTo

--------------------------------------------------------------------------------
-- Resolution algorithm

resolveAssembly :: Assembly (Module ()) -> CompilePure (Assembly (Module RS))
resolveAssembly = go . mapModules NodeId.assign
 where
  go asm = do
    ensureUniqueModuleNames asm
    sharedCtx <- buildSharedResolveCtx asm
    forModulesM asm $ \modul -> do
      let ctx = buildLocalResolveCtx sharedCtx modul
      evalStateT resolveCurrentModule ctx

ensureUniqueModuleNames :: Assembly (Module NodeId) -> CompilePure ()
ensureUniqueModuleNames asm =
  flip evalStateT HS.empty . forM_ (asm ^. assemblyModules) $ \m -> do
    let name = m ^. moduleName
    isDuplicate <- use $ contains name
    when isDuplicate . lift $ pushErrorFor m (errDupModule name)
    modify $ HS.insert name

resolveCurrentModule :: ResolveM (Module RS)
resolveCurrentModule =
  uses currentModule (fmap (, NotApplicable))
    >>= (moduleExport %%~ mapM resolveExportDecl)
    >>= (importPrelude $>)
    >>= (moduleItems %%~ mapM resolveItemDecl)
    >>= (moduleItems %%~ mapM resolveItemDeclBodies)

importPrelude :: ResolveM ()
importPrelude = do
  let prelude = ModuleName "prelude"
  m <- use currentModule
  unless (m ^. moduleName == prelude) $ do
    ge <- use globalExports
    case ge ^. at prelude of
      Nothing -> lift $ pushErrorFor m errNoPrelude
      Just hs -> do
        define m prelude prelude
        forM_ hs $ \iName -> define m iName (ToItem prelude iName)

resolveExportDecl :: ExportDecl RS -> ResolveM (ExportDecl RS)
resolveExportDecl ed = do
  modName     <- use $ currentModule . moduleName
  myItemNames <- mapMaybe (view itemName) <$> use (currentModule . moduleItems)
  ed & exportDeclItems %%~ mapM
    (\sid -> do
      let itName = ItemName $ sid ^. unScopeId
      if itName `elem` myItemNames
        then return (sid & resolution .~ ResolvedItem modName itName)
        else lift . pushErrorFor sid $ errModuleExportsUndefined itName
    )

resolveItemDecl :: ItemDecl RS -> ResolveM (ItemDecl RS)
resolveItemDecl it = case it ^. itemDeclKind of
  ImportItemDecl d -> go resolveImportDecl ImportItemDecl d
  FunItemDecl    d -> go resolveFunDecl FunItemDecl d
 where
  go
    :: (Annotated a)
    => (a RS -> ResolveM (a RS))
    -> (a RS -> ItemDeclKind RS)
    -> a RS
    -> ResolveM (ItemDecl RS)
  go f c d = f d <&> \d' -> it & (itemDeclKind .~ c d')

resolveImportDecl :: ImportDecl RS -> ResolveM (ImportDecl RS)
resolveImportDecl imp = case imp ^. importDeclKind of
  ImportQid q -> do
    let mName = q ^. qidMod & ModuleName
    let iName = q ^. qidScope & ItemName
    ge <- use globalExports
    case ge ^. at mName of
      Just hs -> unless (hs ^. contains iName) $ do
        lift . pushErrorFor q $ errUnknownQid mName iName
      Nothing -> lift . pushErrorFor q $ errUnknownModule mName
    define imp q (ToItem mName iName)
    return $ imp & importDeclKind .~ ImportQid
      (q & resolution .~ ResolvedItem mName iName)

  ImportQidAs q a -> do
    let mName = q ^. qidMod & ModuleName
    let iName = q ^. qidScope & ItemName
    ge <- use globalExports
    case ge ^. at mName of
      Just hs -> unless (hs ^. contains iName) $ do
        lift . pushErrorFor q $ errUnknownQid mName iName
      Nothing -> lift . pushErrorFor q $ errUnknownModule mName
    define imp a (ToItem mName iName)
    return $ imp & importDeclKind .~ ImportQidAs
      (q & resolution .~ ResolvedItem mName iName)
      (a & resolution .~ ResolvedItem mName iName)

  ImportId m -> do
    let mName = toName m
    ge <- use globalExports
    when (at mName `hasn't` ge) $ do
      lift . pushErrorFor m $ errUnknownModule mName
    define imp m mName
    return $ imp & importDeclKind .~ ImportId
      (m & resolution .~ ResolvedModule mName)

  ImportIdAs m a -> do
    let mName = toName m
    ge <- use globalExports
    when (at mName `hasn't` ge) $ do
      lift . pushErrorFor m $ errUnknownModule mName
    define imp a mName
    return $ imp & importDeclKind .~ ImportIdAs
      (m & resolution .~ ResolvedModule mName)
      (a & resolution .~ ResolvedModule mName)

  ImportAll m -> do
    let mName = toName m
    ge <- use globalExports
    case ge ^. at mName of
      Nothing -> lift . pushErrorFor m $ errUnknownModule mName
      Just hs -> do
        define imp m mName
        forM_ hs $ \iName -> define imp iName (ToItem mName iName)
        return $ imp & importDeclKind .~ ImportAll
          (m & resolution .~ ResolvedModule mName)

resolveFunDecl :: FunDecl RS -> ResolveM (FunDecl RS)
resolveFunDecl fn = do
  mName <- use $ currentModule . moduleName
  let funName = fn ^. funDeclName
  let itName  = funName ^. unScopeId & ItemName
  define fn funName $ ToItem mName itName
  return $ fn & funDeclName . resolution .~ ResolvedItem mName itName

resolveItemDeclBodies :: ItemDecl RS -> ResolveM (ItemDecl RS)
resolveItemDeclBodies it = case it ^. itemDeclKind of
  FunItemDecl d -> go resolveFunDeclBodies FunItemDecl d
  _             -> return it
 where
  go
    :: (Annotated a)
    => (a RS -> ResolveM (a RS))
    -> (a RS -> ItemDeclKind RS)
    -> a RS
    -> ResolveM (ItemDecl RS)
  go f c d = f d <&> \d' -> it & (itemDeclKind .~ c d')

resolveFunDeclBodies :: FunDecl RS -> ResolveM (FunDecl RS)
resolveFunDeclBodies fn = do
  let funName = fn ^. funDeclName
  withScope (mkScopeN @ValueName ItemScope funName)
    $   fn
    &   (funDeclParams %%~ mapM resolveFunParam)
    >>= (funDeclBody . funBodyBlock %%~ resolveBlock)

resolveFunParam :: FunParam RS -> ResolveM (FunParam RS)
resolveFunParam p = do
  define p (p ^. funParamId) $ ToNodeId (p ^. funParamId . nodeId)
  return $ p & funParamId . resolution .~ ResolvedLocal
    (p ^. funParamId . nodeId)

resolveBlock :: Block RS -> ResolveM (Block RS)
resolveBlock = blockStmts %%~ mapM resolveStmt

resolveStmt :: Stmt RS -> ResolveM (Stmt RS)
resolveStmt s = case s ^. stmtKind of
  AssignStmt sid expr -> do
    resolvedToNodeId <-
      lookupOrDefine sid sid (ToNodeId (sid ^. nodeId)) >>= \case
        ToNodeId ni        -> return ni
        ToItem mName iName -> do
          void . lift . pushWarningFor s $ warnItemShadow sid mName iName
          redefine sid sid $ ToNodeId (sid ^. nodeId)
          return (sid ^. nodeId)
    let sid' = sid & resolution .~ ResolvedLocal resolvedToNodeId
    expr' <- resolveExpr expr
    return $ s & stmtKind .~ AssignStmt sid' expr'

  ExprStmt expr -> do
    expr' <- resolveExpr expr
    return $ s & stmtKind .~ ExprStmt expr'

resolveExpr :: Expr RS -> ResolveM (Expr RS)
resolveExpr e = case e ^. exprKind of
  IdExpr (ScopeId' sid) -> lookup sid >>= \case
    Just rt ->
      let sid' = sid & resolution .~ convert rt
      in  return $ e & exprKind .~ IdExpr (ScopeId' sid')
    Nothing -> lift . pushErrorFor sid $ errUndefinedSymbol sid

  IdExpr (Qid' qid) ->
    let mName' = qid ^. qidMod & ModuleName
        iName  = qid ^. qidScope & ItemName
    in  lookup mName' >>= \case
          Nothing    -> lift . pushErrorFor qid $ errUnknownModule mName'
          Just mName -> do
            ge <- use globalExports
            let hs = ge ^. at mName ^?! _Just
            if hs ^. contains iName
              then
                let qid' = qid & resolution .~ ResolvedItem mName iName
                in  return $ e & exprKind .~ IdExpr (Qid' qid')
              else lift . pushErrorFor qid $ errUnknownQid mName iName

  LitExpr   _     -> return e

  BlockExpr block -> do
    block' <- resolveBlock block
    return $ e & exprKind .~ BlockExpr block'

  SuccExpr expr -> do
    expr' <- resolveExpr expr
    return $ e & exprKind .~ SuccExpr expr'

  FailExpr expr -> do
    expr' <- resolveExpr expr
    return $ e & exprKind .~ FailExpr expr'

  UnExpr o expr -> do
    expr' <- resolveExpr expr
    return $ e & exprKind .~ UnExpr o expr'

  BinExpr o lhs rhs -> do
    lhs' <- resolveExpr lhs
    rhs' <- resolveExpr rhs
    return $ e & exprKind .~ BinExpr o lhs' rhs'

  CallExpr expr args -> do
    expr' <- resolveExpr expr
    args' <- mapM resolveExpr args
    return $ e & exprKind .~ CallExpr expr' args'

  WhileExpr cond block ->
    withScope (mkScopeT @ValueName VariableScope "while" e) $ do
      cond' <- resolveExpr cond
      currentScope @ValueName . scopeWriteTransparent .= True
      block' <- resolveBlock block
      return $ e & exprKind .~ WhileExpr cond' block'

  IfExpr cond ifBlock elseBlockOpt ->
    withScope (mkScopeT @ValueName VariableScope "if" e) $ do
      cond' <- resolveExpr cond
      currentScope @ValueName . scopeWriteTransparent .= True
      ifBlock'      <- resolveBlock ifBlock
      elseBlockOpt' <- mapM resolveBlock elseBlockOpt
      return $ e & exprKind .~ IfExpr cond' ifBlock' elseBlockOpt'

  ParenExpr expr -> do
    expr' <- resolveExpr expr
    return $ e & exprKind .~ ParenExpr expr'

  ReturnExpr exprOpt -> do
    exprOpt' <- mapM resolveExpr exprOpt
    return $ e & exprKind .~ ReturnExpr exprOpt'


--------------------------------------------------------------------------------
-- Resolve context creation

buildLocalResolveCtx :: SharedResolveCtx -> Module NodeId -> ResolveCtx
buildLocalResolveCtx sctx modul =
  let mName               = modul ^. moduleName
      _globalExports      = sctx ^. sharedGlobalExports
      _currentModule      = modul
      getValueScopeStack  = emptyScopeStack mName
      getModuleScopeStack = emptyScopeStack mName
  in  ResolveCtx { .. }

buildSharedResolveCtx
  :: Assembly (Module NodeId) -> CompilePure SharedResolveCtx
buildSharedResolveCtx asm = SharedResolveCtx <$> buildExportNameMap asm

buildExportNameMap
  :: Assembly (Module NodeId)
  -> CompilePure (HMS.HashMap ModuleName (HashSet ItemName))
buildExportNameMap asm =
  HMS.fromList . zip (mods <&> view moduleName) <$> mapM collectExports mods
 where
  mods = asm ^. assemblyModules & toList

  collectExports m = execStateT (mapM procExport $ getExportDecls m) HS.empty

  getExportDecls m = case m ^. moduleExport of
    Nothing -> []
    Just e  -> e ^. exportDeclItems

  procExport sid = do
    let iName = convert sid
    isDuplicate <- use $ contains iName
    when isDuplicate . lift $ pushWarningFor sid (warnDupExport iName)
    modify $ HS.insert iName

--------------------------------------------------------------------------------
-- Error messages

errAlreadyDefined :: (Name n, HasSourcePos p) => n -> p -> Text
errAlreadyDefined name pos =
  quoted (showName name)
    <> " has been already defined at "
    <> (pos ^. sourcePos & diagnosticShow)
    <> "."

errDupModule :: ModuleName -> Text
errDupModule (ModuleName n) = "Duplicate module " <> quoted n <> "."

errModuleExportsUndefined :: ItemName -> Text
errModuleExportsUndefined (ItemName n) =
  "Module exports item " <> quoted n <> " but does not define it."

errNoPrelude :: Text
errNoPrelude = "No module named 'prelude' in current module path."

errUndefinedSymbol :: ScopeId a -> Text
errUndefinedSymbol sid =
  "Undefined symbol " <> quoted (sid ^. unScopeId) <> "."

errUnknownModule :: ModuleName -> Text
errUnknownModule (ModuleName n) = "Unknown module " <> quoted n <> "."

errUnknownQid :: ModuleName -> ItemName -> Text
errUnknownQid (ModuleName m) (ItemName i) =
  "Unknown item " <> quoted (m <> ":" <> i) <> "."

warnDupExport :: ItemName -> Text
warnDupExport (ItemName n) =
  "Duplicate export of same item " <> quoted n <> "."

warnItemShadow :: ScopeId a -> ModuleName -> ItemName -> Text
warnItemShadow sid (ModuleName m) (ItemName n) =
  "Variable "
    <> quoted (sid ^. unScopeId)
    <> " shadows item "
    <> quoted (m <> ":" <> n)
    <> "."

quoted :: Text -> Text
quoted n = ('\'' <| n) |> '\''
