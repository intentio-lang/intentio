module Intentio.Resolver
  ( Resolution(..)
  , HasResolution(..)
  , RS
  , _ResolvedItem
  , _ResolvedLocal
  , _Unresolved
  , _NotApplicable
  , resolve
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
                                                , pushErrorFor
                                                , pushIceFor
                                                , pushWarningFor
                                                )
import           Intentio.Util.NodeId           ( NodeId
                                                , nodeId
                                                )
import           Language.Intentio.AST

--------------------------------------------------------------------------------
-- Types

data KnownName
  = KnownQid Text Text
  | KnownScopeId Text
  deriving (Show, Eq, Generic)

instance Hashable KnownName

data ScopeKind
  = GlobalScope
  | ItemScope
  | VariableScope
  deriving (Show, Eq)

type ScopeName = Text

data Scope = Scope
  { _scopeKind :: ScopeKind
  , _scopeName :: ScopeName
  , _scopeIds  :: HMS.HashMap KnownName NodeId
  }
  deriving (Show, Eq)

data SharedResolveCtx = SharedResolveCtx
  { _sharedGlobalExports     :: HMS.HashMap ModuleName (HashSet ItemName)
  , _sharedGlobalItemNameMap :: HMS.HashMap (ModuleName, ItemName) NodeId
  }
  deriving (Show, Eq)

data ResolveCtx = ResolveCtx
  { _globalExports     :: HMS.HashMap ModuleName (HashSet ItemName)
  , _globalItemNameMap :: HMS.HashMap (ModuleName, ItemName) NodeId
  , _currentModule     :: Module NodeId
  , _scopeStack        :: NonEmpty Scope
  }
  deriving (Show, Eq)

type ResolveM a = StateT ResolveCtx CompilePure a

type RS = (NodeId, Resolution)

data Resolution
  = ResolvedItem ModuleName ItemName
  | ResolvedLocal NodeId
  | Unresolved
  | NotApplicable
  deriving (Show, Eq)

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

makePrisms ''Resolution
makeLenses ''Scope
makeLenses ''SharedResolveCtx
makeLenses ''ResolveCtx

showScopeKind :: ScopeKind -> Text
showScopeKind GlobalScope   = "g"
showScopeKind ItemScope     = "i"
showScopeKind VariableScope = "v"

showScopeName :: Scope -> Text
showScopeName s = (s ^. scopeName) <> "/" <> (s ^. scopeKind & showScopeKind)

emptyScopeStack :: ModuleName -> NonEmpty Scope
emptyScopeStack (ModuleName n) = Scope GlobalScope n mempty :| []

currentScope :: ResolveM Scope
currentScope = uses scopeStack NE.head

pushScope :: ScopeKind -> ScopeName -> ResolveM ()
pushScope GlobalScope _ = do
  m <- use currentModule
  lift $ pushIceFor m "Cannot push global scope"
pushScope k n = scopeStack %= NE.cons (Scope k n mempty)

popScope :: ResolveM ()
popScope = uses scopeStack NE.uncons >>= \case
  (_, Just st) -> scopeStack .= st
  (_, Nothing) -> do
    m <- use currentModule
    lift $ pushIceFor m "Cannot pop global scope"

withScope :: ScopeKind -> ScopeName -> ResolveM a -> ResolveM a
withScope n k f = pushScope n k *> f <* popScope

--------------------------------------------------------------------------------
-- Resolution algorithm

resolve :: Assembly (Module NodeId) -> CompilePure (Assembly (Module RS))
resolve asm = do
  ensureUniqueModuleNames asm
  sharedCtx <- buildSharedResolveCtx asm
  forModulesM asm $ \modul -> do
    let ctx = buildLocalResolveCtx sharedCtx modul
    evalStateT processCurrentModule ctx

ensureUniqueModuleNames :: Assembly (Module NodeId) -> CompilePure ()
ensureUniqueModuleNames asm =
  flip evalStateT HS.empty . forM_ (asm ^. assemblyModules) $ \m -> do
    let name = m ^. moduleName
    isDuplicate <- use $ contains name
    when isDuplicate . lift $ pushErrorFor m (errDupModule name)
    modify $ HS.insert name

processCurrentModule :: ResolveM (Module RS)
processCurrentModule =
  uses currentModule (fmap (, NotApplicable))
    >>= (moduleExport #%%~ mapM processExportDecl)
    >>= (moduleItems #%%~ mapM processItemDecl)

processExportDecl :: ExportDecl RS -> ResolveM (ExportDecl RS)
processExportDecl = exportDeclItems #%%~ mapM processId
 where
  processId sid = do
    let itName = ItemName $ sid ^. unScopeId
    modName <- use $ currentModule . moduleName
    use (globalItemNameMap . at (modName, itName)) >>= \case
      Just _  -> return (sid & resolution .~ ResolvedItem modName itName)
      Nothing -> lift . pushErrorFor sid $ errModuleExportsUndefined itName

processItemDecl :: ItemDecl RS -> ResolveM (ItemDecl RS)
processItemDecl = undefined

--------------------------------------------------------------------------------
-- Resolve context creation

buildLocalResolveCtx :: SharedResolveCtx -> Module NodeId -> ResolveCtx
buildLocalResolveCtx sctx modul =
  let _globalExports     = sctx ^. sharedGlobalExports
      _globalItemNameMap = sctx ^. sharedGlobalItemNameMap
      _currentModule     = modul
      _scopeStack        = emptyScopeStack $ modul ^. moduleName
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

errDupModule :: ModuleName -> Text
errDupModule (ModuleName n) = "Duplicate module with same name: " <> n

errDupName :: ItemName -> Text
errDupName (ItemName n) = "Multiple definitions of item: " <> n

errModuleExportsUndefined :: ItemName -> Text
errModuleExportsUndefined (ItemName n) =
  "Module exports item '" <> n <> "' but does not define it."

warnDupExport :: ItemName -> Text
warnDupExport (ItemName n) = "Duplicate export of same item: " <> n
