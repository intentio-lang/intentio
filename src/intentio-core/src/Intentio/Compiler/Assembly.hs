module Intentio.Compiler.Assembly
  (-- * Assembly data structure
    Assembly
  , AssemblyName
  , AssemblyType(..)
  , assemblyType
  , assemblyName
  , assemblyMainModuleName
  , assemblyModules
   -- ** Constructing assemblies
  , library
  , program
  , mkModuleMap
  , AssemblyConstructError(..)
  , prettyAssemblyConstructError

   -- * Module type class
  , ModuleName
  , Module(..)
  , moduleName
  , moduleItems

   -- * Item type class
  , ItemName
  , Item(..)
  , itemName
  )
where

import           Intentio.Prelude

import qualified Data.Map.Strict               as M

type AssemblyName = Text
type ModuleName = Text
type ItemName = Text

--------------------------------------------------------------------------------
-- Assembly data structure

data AssemblyType = Program | Library
  deriving (Show, Eq, Ord)

data Assembly m
    = MkAssembly
    { _assemblyType           :: AssemblyType
    , _assemblyName           :: AssemblyName
    , _assemblyModules        :: Map ModuleName m
    , _assemblyMainModuleName :: Maybe ModuleName
    }
  deriving (Show, Eq)
makeLenses ''Assembly

data AssemblyConstructError = MainDoesNotExist
  deriving (Show, Eq, Ord)

prettyAssemblyConstructError :: IsString a => AssemblyConstructError -> a
prettyAssemblyConstructError MainDoesNotExist = "main module does not exist"

library :: Module m => AssemblyName -> NonEmpty m -> Assembly m
library name modlist = MkAssembly
  { _assemblyType           = Library
  , _assemblyName           = name
  , _assemblyModules        = mkModuleMap modlist
  , _assemblyMainModuleName = Nothing
  }

program :: Module m => AssemblyName -> NonEmpty m -> Assembly m
program name modlist = MkAssembly
  { _assemblyType           = Program
  , _assemblyName           = name
  , _assemblyModules        = mkModuleMap modlist
  , _assemblyMainModuleName = Just $ mainMod modlist
  }

mainMod :: Module m => NonEmpty m -> ModuleName
mainMod (main :| _) = main ^. moduleName

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
