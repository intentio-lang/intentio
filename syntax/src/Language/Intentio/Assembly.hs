module Language.Intentio.Assembly
  (-- * Assembly data structure
    Assembly
  , AssemblyName(..)
  , unAssemblyName
  , AssemblyType(..)
  , showAssemblyTypeAbbr
  , fromAssemblyTypeAbbr
  , assemblyType
  , assemblyName
  , assemblyOutputPath
  , assemblyMainModuleName
  , assemblyModules
   -- ** Constructing assemblies
  , mkAssembly
  , mkModuleMap
  , AssemblyConstructError(..)
  , prettyAssemblyConstructError
   -- ** Resolving modules & items
  , lookupModule
  , lookupMainModule
  , lookupItem
   -- ** Operations on assemblies
  , mapModules
  , mapModulesM
  , forModulesM
  , mapModulesM_
  , forModulesM_
  , concatMapModules
  , concatMapModulesM

   -- * Module type class
  , ModuleName(..)
  , unModuleName
  , Module(..)
  , moduleName
  , moduleItems
  , moduleLookupItem

   -- * Item type class
  , ItemName(..)
  , unItemName
  , Item(..)
  , itemName
  )
where

import           Intentio.Prelude

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , ToJSONKey
                                                , FromJSONKey
                                                )
import qualified Data.Map.Strict               as M

import           Language.Intentio.SourcePos    ( HasSourcePos )

newtype AssemblyName = AssemblyName { _unAssemblyName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON AssemblyName where
  toJSON     = toJSON . _unAssemblyName
  toEncoding = toEncoding . _unAssemblyName

instance FromJSON AssemblyName where
  parseJSON v = AssemblyName <$> parseJSON v

instance ToJSONKey AssemblyName
instance FromJSONKey AssemblyName

newtype ModuleName = ModuleName { _unModuleName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON ModuleName where
  toJSON     = toJSON . _unModuleName
  toEncoding = toEncoding . _unModuleName

instance FromJSON ModuleName where
  parseJSON v = ModuleName <$> parseJSON v

instance ToJSONKey ModuleName
instance FromJSONKey ModuleName

newtype ItemName = ItemName { _unItemName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON ItemName where
  toJSON     = toJSON . _unItemName
  toEncoding = toEncoding . _unItemName

instance FromJSON ItemName where
  parseJSON v = ItemName <$> parseJSON v

instance ToJSONKey ItemName
instance FromJSONKey ItemName

makeLenses ''AssemblyName
makeWrapped ''AssemblyName
makeLenses ''ModuleName
makeWrapped ''ModuleName
makeLenses ''ItemName
makeWrapped ''ItemName

--------------------------------------------------------------------------------
-- Assembly data structure

data AssemblyType = Program | Library
  deriving (Show, Eq, Ord, Generic)

instance ToJSON AssemblyType
instance FromJSON AssemblyType

showAssemblyTypeAbbr :: IsString a => AssemblyType -> a
showAssemblyTypeAbbr Program = "bin"
showAssemblyTypeAbbr Library = "lib"

fromAssemblyTypeAbbr :: (IsString a, Eq a) => a -> Maybe AssemblyType
fromAssemblyTypeAbbr "bin" = Just Program
fromAssemblyTypeAbbr "lib" = Just Library
fromAssemblyTypeAbbr _     = Nothing

data Assembly m
    = MkAssembly
    { _assemblyType           :: AssemblyType
    , _assemblyName           :: AssemblyName
    , _assemblyOutputPath     :: FilePath
    , _assemblyModules        :: Map ModuleName m
    , _assemblyMainModuleName :: Maybe ModuleName
    }
  deriving (Show, Eq, Generic)

makeLenses ''Assembly

instance ToJSON m => ToJSON (Assembly m)
instance FromJSON m => FromJSON (Assembly m)

data AssemblyConstructError = MainDoesNotExist
  deriving (Show, Eq, Ord)

prettyAssemblyConstructError :: IsString a => AssemblyConstructError -> a
prettyAssemblyConstructError MainDoesNotExist = "main module does not exist"

mkAssembly
  :: Module m
  => AssemblyType
  -> AssemblyName
  -> FilePath
  -> NonEmpty m
  -> Assembly m
mkAssembly _assemblyType _assemblyName _assemblyOutputPath modlist = MkAssembly
  { _assemblyType
  , _assemblyName
  , _assemblyOutputPath
  , _assemblyModules        = mkModuleMap modlist
  , _assemblyMainModuleName
  }
 where
  _assemblyMainModuleName = mainMod _assemblyType modlist

  mainMod Program (main :| _) = Just $ main ^. moduleName
  mainMod _       _           = Nothing

mkModuleMap :: Module m => NonEmpty m -> Map ModuleName m
mkModuleMap = M.fromList . fmap (\m -> (_moduleName m, m)) . toList

lookupModule :: Module a => ModuleName -> Assembly a -> Maybe a
lookupModule n a = a ^. assemblyModules . at n

lookupMainModule :: Module a => Assembly a -> Maybe a
lookupMainModule a = a ^. assemblyMainModuleName >>= flip lookupModule a

lookupItem
  :: Module a => ModuleName -> ItemName -> Assembly a -> Maybe (a, ItemTy a)
lookupItem m i a = do
  modul <- lookupModule m a
  item  <- modul ^. moduleLookupItem i
  return (modul, item)

mapModules :: (Module a, Module b) => (a -> b) -> Assembly a -> Assembly b
mapModules f srcAsm =
  let srcMl = srcAsm ^. assemblyModules & toList
      dstMl = fmap f srcMl
      dstMm = mkModuleMap $ fromList dstMl
  in  srcAsm & assemblyModules .~ dstMm

mapModulesM
  :: (Module a, Module b, Monad m) => (a -> m b) -> Assembly a -> m (Assembly b)
mapModulesM f srcAsm = do
  let srcMl = srcAsm ^. assemblyModules & toList
  dstMl <- mapM f srcMl
  let dstMm = mkModuleMap $ fromList dstMl
  return $ srcAsm & assemblyModules .~ dstMm

forModulesM
  :: (Module a, Module b, Monad m) => Assembly a -> (a -> m b) -> m (Assembly b)
forModulesM = flip mapModulesM

mapModulesM_ :: (Module a, Monad m) => (a -> m b) -> Assembly a -> m ()
mapModulesM_ f srcAsm = mapM_ f (srcAsm ^. assemblyModules & toList)

forModulesM_ :: (Module a, Monad m) => Assembly a -> (a -> m b) -> m ()
forModulesM_ = flip mapModulesM_

concatMapModules
  :: (Module a, Module b) => (a -> [b]) -> Assembly a -> Assembly b
concatMapModules f srcAsm =
  let srcMl = srcAsm ^. assemblyModules & toList
      dstMl = fmap f srcMl
      dstMm = mkModuleMap . fromList . concat $ dstMl
  in  srcAsm & assemblyModules .~ dstMm

concatMapModulesM
  :: (Module a, Module b, Monad m)
  => (a -> m [b])
  -> Assembly a
  -> m (Assembly b)
concatMapModulesM f srcAsm = do
  let srcMl = srcAsm ^. assemblyModules & toList
  dstMl <- mapM f srcMl
  let dstMm = mkModuleMap . fromList . concat $ dstMl
  return $ srcAsm & assemblyModules .~ dstMm

--------------------------------------------------------------------------------
-- Module class

class (Eq a, Show a, HasSourcePos a, Item (ItemTy a)) => Module a where
  -- | A type of items declared by this module type.
  type ItemTy a

  -- | Name of this module.
  _moduleName :: a -> ModuleName

  -- | A list of items declared in this module.
  --
  -- Items should be in same order as in input source code.
  _moduleItems :: a -> [ItemTy a]

  -- | Lookup item by name.
  --
  -- This operation may be usually asymptotically faster than `O(n)`.
  _moduleLookupItem :: ItemName -> a -> Maybe (ItemTy a)
  _moduleLookupItem n = find (\i -> _itemName i == Just n) . _moduleItems
  {-# INLINABLE _moduleLookupItem #-}

moduleName
  :: (Profunctor p, Contravariant f, Module a) => Optic' p f a ModuleName
moduleName = to _moduleName
{-# INLINE moduleName #-}

moduleItems
  :: (Profunctor p, Contravariant f, Module a) => Optic' p f a [ItemTy a]
moduleItems = to _moduleItems
{-# INLINE moduleItems #-}

moduleLookupItem
  :: (Profunctor p, Contravariant f, Module a)
  => ItemName
  -> Optic' p f a (Maybe (ItemTy a))
moduleLookupItem iName = to $ _moduleLookupItem iName
{-# INLINE moduleLookupItem #-}

instance Module Void where
  type ItemTy Void = Void
  _moduleName       = unreachable
  _moduleItems      = unreachable
  _moduleLookupItem = unreachable

--------------------------------------------------------------------------------
-- Item class

class (Eq a, Show a, HasSourcePos a) => Item a where
  -- | Name of this item, visible to outer world.
  _itemName :: a -> Maybe ItemName

itemName
  :: (Profunctor p, Contravariant f, Item a) => Optic' p f a (Maybe ItemName)
itemName = to _itemName
{-# INLINE itemName #-}

instance Item Void where
  _itemName = unreachable
