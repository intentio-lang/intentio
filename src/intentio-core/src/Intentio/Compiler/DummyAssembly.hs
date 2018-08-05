module Intentio.Compiler.DummyAssembly
  ( -- * Dummy assembly
    DummyAssembly
  , dummyAssembly
  , dummyAssembly'
  , dummyAssembly1

    -- * Dummy module
  , DummyModule(..)
  , dummyModuleName
  , dummyModuleItems
  , dummyModule
  , dummyModule'

    -- * Dummy item
  , DummyItem(..)
  , dummyItemName
  , dummyItem
  )
where

import           Intentio.Prelude

import           Intentio.Compiler.Assembly     ( Assembly
                                                , library
                                                , Module(..)
                                                , ModuleName
                                                , Item(..)
                                                , ItemName
                                                )

-- | An 'Assembly' that does not do anything special. Useful for testing
-- or as a seed in complex compilation flows.
type DummyAssembly = Assembly DummyModule

-- | A library 'DummyAssembly' named @dummy_assembly@ with single 'DummyModule'
-- named @dummy_module@.
dummyAssembly :: DummyAssembly
dummyAssembly = dummyAssembly' (dummyModule :| [])

-- | Constructs library 'DummyAssembly' named @dummy_assembly@ with specified
-- list of dummy modules.
dummyAssembly' :: NonEmpty DummyModule -> DummyAssembly
dummyAssembly' = library "dummy_assembly"

-- | A library 'DummyAssembly' named @dummy_assembly@ with single `DummyModule'
-- named @dummy_module@ containing one 'DummyItem' named @dummy_item@.
dummyAssembly1 :: DummyAssembly
dummyAssembly1 = dummyAssembly' $ dummyModule' [dummyItem] :| []

-- | An 'Module' that does not do anything special.
data DummyModule = DummyModule { _dummyModuleName :: ModuleName, _dummyModuleItems :: [DummyItem] }
  deriving (Show, Eq)

instance Module DummyModule where
  type ItemTy DummyModule = DummyItem
  _moduleName = _dummyModuleName
  _moduleItems = _dummyModuleItems

-- | A 'DummyModule' named @DummyModule@ with no items.
dummyModule :: DummyModule
dummyModule = dummyModule' []

-- | Constructs 'DummyModule' named @dummy_module@ with specified list of items.
dummyModule' :: [DummyItem] -> DummyModule
dummyModule' = DummyModule "dummy_module"

-- | An 'Item' that does not do anything special.
newtype DummyItem = DummyItem { _dummyItemName :: ItemName }
  deriving (Show, Eq)

instance Item DummyItem where
  _itemName = _dummyItemName

-- | A 'DummyItem' named @dummy_item@.
dummyItem :: DummyItem
dummyItem = DummyItem "dummy_item"

makeLenses ''DummyModule
makeLenses ''DummyItem
