module Intentio.Resolver where

import           Intentio.Prelude

import qualified Data.HashMap.Strict           as HMS
import qualified Data.HashSet                  as HS

import           Intentio.Annotated             ( ann )
import           Intentio.Compiler              ( Assembly
                                                , CompilePure
                                                , assemblyModules
                                                , itemName
                                                , moduleItems
                                                , moduleName
                                                , pushErrorFor
                                                , unItemName
                                                )
import           Intentio.Util.NodeId           ( NodeId )
import           Language.Intentio.AST          ( ModuleName(..)
                                                , ItemName(..)
                                                )
import qualified Language.Intentio.AST         as A

data ItemIndex = ItemIndex
  { _importNameMap :: HMS.HashMap ModuleName (HashSet ItemName)
  , _itemNameMap   :: HMS.HashMap (ModuleName, ItemName) NodeId
  }
  deriving (Show, Eq)

makeLenses ''ItemIndex

buildItemIndex :: Assembly (A.Module NodeId) -> CompilePure ItemIndex
buildItemIndex = undefined

buildItemNameMap
  :: Assembly (A.Module NodeId)
  -> CompilePure (HMS.HashMap (ModuleName, ItemName) NodeId)
buildItemNameMap asm = HMS.fromList . concat <$> mapM procMod mods
 where
  mods = asm ^. assemblyModules & toList

  procMod :: A.Module NodeId -> CompilePure [((ModuleName, ItemName), NodeId)]
  procMod m = fmap (key $ m ^. moduleName) <$> collItems m

  key :: ModuleName -> (ItemName, NodeId) -> ((ModuleName, ItemName), NodeId)
  key m (k, v) = ((m, k), v)

  collItems :: A.Module NodeId -> CompilePure [(ItemName, NodeId)]
  collItems m =
    catMaybes <$> evalStateT (mapM procItem $ m ^. moduleItems) HS.empty

  procItem item = case item ^. itemName of
    Nothing    -> pure Nothing
    Just iName -> do
      isDuplicate <- use $ contains iName
      when isDuplicate . lift $ pushErrorFor item (errDupName iName)
      modify $ HS.insert iName
      return $ Just (iName, item ^. ann)

  errDupName n = "Multiple definitions of item \"" <> n ^. unItemName <> "\"."
