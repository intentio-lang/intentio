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
  , librarySafe
  , program
  , programSafe
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

import           Intentio.Prelude        hiding ( moduleName )

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
library n l = case librarySafe n l of
  Right x -> x
  Left  e -> error $ prettyAssemblyConstructError e

librarySafe
  :: Module m
  => AssemblyName
  -> NonEmpty m
  -> Either AssemblyConstructError (Assembly m)
librarySafe name modlist = Right $ MkAssembly
  { _assemblyType           = Library
  , _assemblyName           = name
  , _assemblyModules        = modListToMap modlist
  , _assemblyMainModuleName = Nothing
  }

program :: Module m => AssemblyName -> ModuleName -> NonEmpty m -> Assembly m
program n m l = case programSafe n m l of
  Right x -> x
  Left  e -> error $ prettyAssemblyConstructError e

programSafe
  :: Module m
  => AssemblyName
  -> ModuleName
  -> NonEmpty m
  -> Either AssemblyConstructError (Assembly m)
programSafe name main modlist = if M.member main modmap
  then Right $ MkAssembly
    { _assemblyType           = Program
    , _assemblyName           = name
    , _assemblyModules        = modmap
    , _assemblyMainModuleName = Just main
    }
  else Left MainDoesNotExist
  where modmap = modListToMap modlist

modListToMap :: Module m => NonEmpty m -> Map ModuleName m
modListToMap = M.fromList . map (\m -> (_moduleName m, m)) . toList

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

--------------------------------------------------------------------------------
-- Item class

class (Eq a, Show a) => Item a where
  _itemName :: a -> ItemName

itemName :: (Profunctor p, Contravariant f, Item a) => Optic' p f a ItemName
itemName = to _itemName
