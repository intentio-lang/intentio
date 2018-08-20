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

import qualified Data.Map.Strict               as M

newtype AssemblyName = AssemblyName { _unAssemblyName :: Text }
  deriving (Show, Eq, Ord, Hashable)

newtype ModuleName = ModuleName { _unModuleName :: Text }
  deriving (Show, Eq, Ord, Hashable)

newtype ItemName = ItemName { _unItemName :: Text }
  deriving (Show, Eq, Ord, Hashable)

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

class (Eq a, Show a, Item (ItemTy a)) => Module a where
  type ItemTy a
  _moduleName :: a -> ModuleName
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
  _moduleItems = const []

--------------------------------------------------------------------------------
-- Item class

class (Eq a, Show a) => Item a where
  _itemName :: a -> ItemName

itemName :: (Profunctor p, Contravariant f, Item a) => Optic' p f a ItemName
itemName = to _itemName

instance Item Void where
  _itemName = unreachable
