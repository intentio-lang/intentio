module Language.Intentio.AssemblySpec where

import           Intentio.Prelude

import           Test.Hspec

import           Language.Intentio.Assembly
import           Language.Intentio.SourcePos

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
dummyAssembly' =
  mkAssembly Library (AssemblyName "dummy_assembly") "dummy_assembly"

-- | A library 'DummyAssembly' named @dummy_assembly@ with single `DummyModule'
-- named @dummy_module@ containing one 'DummyItem' named @dummy_item@.
dummyAssembly1 :: DummyAssembly
dummyAssembly1 = dummyAssembly' $ dummyModule' [dummyItem] :| []

-- | An 'Module' that does not do anything special.
data DummyModule
    = DummyModule
    { _dummyModuleName :: ModuleName
    , _dummyModuleItems :: [DummyItem]
    }
  deriving (Show, Eq)

instance HasSourcePos DummyModule where
  _sourcePos _ = _sourcePos ()

instance Module DummyModule where
  type ItemTy DummyModule = DummyItem
  _moduleName = _dummyModuleName
  _moduleItems = _dummyModuleItems

-- | A 'DummyModule' named @DummyModule@ with no items.
dummyModule :: DummyModule
dummyModule = dummyModule' []

-- | Constructs 'DummyModule' named @dummy_module@ with specified list of items.
dummyModule' :: [DummyItem] -> DummyModule
dummyModule' = DummyModule (ModuleName "dummy_module")

-- | An 'Item' that does not do anything special.
newtype DummyItem = DummyItem { _dummyItemName :: ItemName }
  deriving (Show, Eq)

instance HasSourcePos DummyItem where
  _sourcePos _ = _sourcePos ()

instance Item DummyItem where
  _itemName = Just . _dummyItemName

-- | A 'DummyItem' named @dummy_item@.
dummyItem :: DummyItem
dummyItem = DummyItem (ItemName "dummy_item")

makeLenses ''DummyModule
makeLenses ''DummyItem

spec :: Spec
spec = parallel $ do
  let aname = AssemblyName "test"

  describe "mkAssembly Library" $ do
    it "should build an library assembly" $ do
      let lib = mkAssembly Library aname "out" (dummyModule :| [])
      lib ^. assemblyName `shouldBe` aname
      lib ^. assemblyOutputPath `shouldBe` "out"

  describe "mkAssembly Program" $ do
    it "should build an program assembly with first module being main" $ do
      let mainName = dummyModule ^. moduleName
      let prog     = mkAssembly Program aname "out" (dummyModule :| [])
      prog ^. assemblyName `shouldBe` aname
      prog ^. assemblyOutputPath `shouldBe` "out"
      prog ^. assemblyMainModuleName `shouldBe` Just mainName
