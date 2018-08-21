module Intentio.Compiler.Assembly
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

   -- * Module type class
  , ModuleName(..)
  , unModuleName
  , Module(..)
  , moduleName
  , moduleItems

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

import           Intentio.Diagnostics           ( SourcePosProvider )

newtype AssemblyName = AssemblyName { _unAssemblyName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON AssemblyName where
  toJSON = toJSON . _unAssemblyName
  toEncoding = toEncoding . _unAssemblyName

instance FromJSON AssemblyName where
  parseJSON v = AssemblyName <$> parseJSON v

instance ToJSONKey AssemblyName
instance FromJSONKey AssemblyName

newtype ModuleName = ModuleName { _unModuleName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON ModuleName where
  toJSON = toJSON . _unModuleName
  toEncoding = toEncoding . _unModuleName

instance FromJSON ModuleName where
  parseJSON v = ModuleName <$> parseJSON v

instance ToJSONKey ModuleName
instance FromJSONKey ModuleName

newtype ItemName = ItemName { _unItemName :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON ItemName where
  toJSON = toJSON . _unItemName
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
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq)
makeLenses ''Assembly

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
mkModuleMap = M.fromList . map (\m -> (_moduleName m, m)) . toList

--------------------------------------------------------------------------------
-- Module class

class (Eq a, Show a, SourcePosProvider a, Item (ItemTy a)) => Module a where
  -- | A type of items declared by this module type.
  type ItemTy a

  -- | Name of this module.
  _moduleName :: a -> ModuleName

  -- | A list of items declared in this module.
  --
  -- Items should be in same order as in input source code.
  _moduleItems :: a -> [ItemTy a]

moduleName
  :: (Profunctor p, Contravariant f, Module a) => Optic' p f a ModuleName
moduleName = to _moduleName

moduleItems
  :: (Profunctor p, Contravariant f, Module a) => Optic' p f a [ItemTy a]
moduleItems = to _moduleItems

instance Module Void where
  type ItemTy Void = Void
  _moduleName = unreachable
  _moduleItems = unreachable

--------------------------------------------------------------------------------
-- Item class

class (Eq a, Show a, SourcePosProvider a) => Item a where
  -- | Name of this item, visible to outer world.
  _itemName :: a -> Maybe ItemName

itemName
  :: (Profunctor p, Contravariant f, Item a) => Optic' p f a (Maybe ItemName)
itemName = to _itemName

instance Item Void where
  _itemName = unreachable
