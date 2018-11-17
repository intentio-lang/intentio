{-# LANGUAGE AllowAmbiguousTypes #-}

module Intentio.Codegen.SymbolNames
  ( GetCModuleFileName(..)
  , cModuleFileNameBase
  , cItemName
  , cItemName'
  , cVarName
  , cVarName'
  , cTmpVarName
  )
where

import           Intentio.Prelude

import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                )
import qualified Intentio.Codegen.Imp.Model    as I
import           Intentio.Codegen.SymbolNames.Mangling
                                                ( mangle )
import           Intentio.Compiler              ( ModuleName(..)
                                                , moduleName
                                                )
import qualified Intentio.Hir                  as H

cModuleFileNameBase :: ModuleName -> FilePath
cModuleFileNameBase (ModuleName modName) = toS $ mangle [modName]

class GetCModuleFileName t where
  cModuleFileName :: ModuleName -> FilePath

instance GetCModuleFileName CModuleSource where
  cModuleFileName m = cModuleFileNameBase m <> ".c"

instance GetCModuleFileName CModuleHeader where
  cModuleFileName m = cModuleFileNameBase m <> ".h"

cItemName :: (Eq a, Show a) => H.Module a -> H.Item a -> String
cItemName modul item = cItemName' (modul ^. moduleName) iname
 where
  iname   = fromMaybe unnamed (item ^. H.itemName)
  unnamed = H.ItemName $ "$" <> (item ^. H.itemId . H.unItemId & show)

cItemName' :: H.ModuleName -> H.ItemName -> String
cItemName' modul item = toS $ mangle [modul ^. _Wrapped, item ^. _Wrapped]

cVarName :: I.Var a -> String
cVarName = cVarName' . view I.varName

cVarName' :: Text -> String
cVarName' = toS . mangle . (: [])

cTmpVarName :: I.VarId -> Text
cTmpVarName i = '%' <| show (i ^. I.unVarId)
