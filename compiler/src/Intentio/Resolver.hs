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
                                                , sourcePos
                                                )
import           Intentio.Util.NodeId           ( NodeId
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
  { _scopeKind :: ScopeKind
  , _scopeName :: ScopeName
  , _scopeIds  :: HMS.HashMap n (ResolvesTo n, SourcePos)
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

instance Name ValueName where
  type ResolvesTo ValueName = ValueResolvesTo
  showName = _unValueName
  scopeStack = lens getValueScopeStack (\c s -> c{getValueScopeStack=s})

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
  showName = _unModuleName
  scopeStack = lens getModuleScopeStack (\c s -> c{getModuleScopeStack=s})

instance ToName ModuleName ModuleName where
  toName = id

instance ToName (ModId a) ModuleName where
  toName = ModuleName . view unModId

--------------------------------------------------------------------------------
-- Name resolver state data types and monads

data SharedResolveCtx = SharedResolveCtx
  { _sharedGlobalExports     :: HMS.HashMap ModuleName (HashSet ItemName)
  , _sharedGlobalItemNameMap :: HMS.HashMap (ModuleName, ItemName) NodeId
  }
  deriving (Show)

data ResolveCtx = ResolveCtx
  { _globalExports      :: HMS.HashMap ModuleName (HashSet ItemName)
  , _globalItemNameMap  :: HMS.HashMap (ModuleName, ItemName) NodeId
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

emptyScopeStack :: forall n . Name n => ModuleName -> NonEmpty (Scope n)
emptyScopeStack (ModuleName n) = Scope GlobalScope n mempty :| []

currentScope :: forall n . Name n => Lens' ResolveCtx (Scope n)
currentScope = lens getter setter
 where
  getter = NE.head . view (scopeStack @n)
  setter c s = c & scopeStack @n %~ \(_ :| ss) -> s :| ss

pushScope :: forall n . Name n => ScopeKind -> ScopeName -> ResolveM ()
pushScope GlobalScope _ = do
  m <- use currentModule
  lift $ pushIceFor m "Cannot push global scope"
pushScope k n = scopeStack @n %= NE.cons (Scope k n mempty)

popScope :: forall n . Name n => ResolveM ()
popScope = uses (scopeStack @n) NE.uncons >>= \case
  (_, Just st) -> scopeStack .= st
  (_, Nothing) -> do
    m <- use currentModule
    lift $ pushIceFor m "Cannot pop global scope"

withScope
  :: forall n a . Name n => ScopeKind -> ScopeName -> ResolveM a -> ResolveM a
withScope n k f = pushScope @n n k *> f <* popScope @n

define
  :: forall n a t
   . (Name n, ToName a n, HasSourcePos t)
  => t
  -> a
  -> ResolvesTo n
  -> ResolveM ()
define target name' resolvesTo = do
  let name = toName name'
  scope <- use currentScope
  case scope ^. scopeIds . at name of
    Just (_, sp) -> lift . pushErrorFor target $ errAlreadyDefined name sp
    Nothing ->
      currentScope . scopeIds . at name ?= (resolvesTo, target ^. sourcePos)

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
    >>= (moduleExport #%%~ mapM resolveExportDecl)
    >>= (moduleItems #%%~ mapM resolveItemDecl)

resolveExportDecl :: ExportDecl RS -> ResolveM (ExportDecl RS)
resolveExportDecl = exportDeclItems #%%~ mapM resolveId
 where
  resolveId sid = do
    let itName = ItemName $ sid ^. unScopeId
    modName <- use $ currentModule . moduleName
    use (globalItemNameMap . at (modName, itName)) >>= \case
      Just _  -> return (sid & resolution .~ ResolvedItem modName itName)
      Nothing -> lift . pushErrorFor sid $ errModuleExportsUndefined itName

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
      Just hs -> when (hs ^. contains iName & not) $ do
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
      Just hs -> when (hs ^. contains iName & not) $ do
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

  ImportAll _ -> lift $ pushIceFor imp "Import-all is not implemented yet."

resolveFunDecl :: FunDecl RS -> ResolveM (FunDecl RS)
resolveFunDecl fn = do
  mName <- use $ currentModule . moduleName
  let funName = fn ^. funDeclName
  let itName  = funName ^. unScopeId & ItemName
  define fn funName $ ToItem mName itName
  return $ fn & funDeclName . resolution .~ ResolvedItem mName itName
  -- withScope @ValueName ItemScope (funName ^. unScopeId) $ do
  --   undefined

--------------------------------------------------------------------------------
-- Resolve context creation

buildLocalResolveCtx :: SharedResolveCtx -> Module NodeId -> ResolveCtx
buildLocalResolveCtx sctx modul =
  let mName               = modul ^. moduleName
      _globalExports      = sctx ^. sharedGlobalExports
      _globalItemNameMap  = sctx ^. sharedGlobalItemNameMap
      _currentModule      = modul
      getValueScopeStack  = emptyScopeStack mName
      getModuleScopeStack = emptyScopeStack mName
  in  ResolveCtx { .. }

buildSharedResolveCtx
  :: Assembly (Module NodeId) -> CompilePure SharedResolveCtx
buildSharedResolveCtx asm =
  SharedResolveCtx <$> buildExportNameMap asm <*> buildItemNameMap asm

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

buildItemNameMap
  :: Assembly (Module NodeId)
  -> CompilePure (HMS.HashMap (ModuleName, ItemName) NodeId)
buildItemNameMap asm = HMS.fromList . concat <$> mapM procMod mods
 where
  mods = asm ^. assemblyModules & toList

  procMod :: Module NodeId -> CompilePure [((ModuleName, ItemName), NodeId)]
  procMod m = fmap (key $ m ^. moduleName) <$> collItems m

  key :: ModuleName -> (ItemName, NodeId) -> ((ModuleName, ItemName), NodeId)
  key m (k, v) = ((m, k), v)

  collItems :: Module NodeId -> CompilePure [(ItemName, NodeId)]
  collItems m =
    catMaybes <$> evalStateT (mapM procItem $ m ^. moduleItems) HS.empty

  procItem item = case item ^. itemName of
    Nothing    -> pure Nothing
    Just iName -> do
      isDuplicate <- use $ contains iName
      when isDuplicate . lift $ pushErrorFor item (errDupName iName)
      modify $ HS.insert iName
      return $ Just (iName, item ^. nodeId)

--------------------------------------------------------------------------------
-- Error messages

errAlreadyDefined :: (Name n, HasSourcePos p) => n -> p -> Text
errAlreadyDefined name pos =
  quoted (showName name)
    <> " has been already defined at "
    <> (pos ^. sourcePos & show)
    <> "."

errDupModule :: ModuleName -> Text
errDupModule (ModuleName n) = "Duplicate module " <> quoted n <> "."

errDupName :: ItemName -> Text
errDupName (ItemName n) = "Multiple definitions of item " <> quoted n <> "."

errModuleExportsUndefined :: ItemName -> Text
errModuleExportsUndefined (ItemName n) =
  "Module exports item " <> quoted n <> " but does not define it."

errUnknownModule :: ModuleName -> Text
errUnknownModule (ModuleName n) = "Unknown module " <> quoted n <> "."

errUnknownQid :: ModuleName -> ItemName -> Text
errUnknownQid (ModuleName m) (ItemName i) =
  "Unknown item " <> quoted (m <> ":" <> i) <> "."

warnDupExport :: ItemName -> Text
warnDupExport (ItemName n) =
  "Duplicate export of same item " <> quoted n <> "."

quoted :: Text -> Text
quoted n = ('\'' <| n) |> '\''
