{-# LANGUAGE AllowAmbiguousTypes #-}

module Intentio.Codegen.SymbolNames
  ( GetCModuleFileName(..)
  , cModuleFileNameBase
  , cItemName
  , cImportedItemName
  , cVarName
  )
where

import           Intentio.Prelude

import           Intentio.Compiler              ( ModuleName(..)
                                                , moduleName
                                                )
import qualified Intentio.Hir                  as H

import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                )
import           Intentio.Codegen.SymbolNames.Mangling
                                                ( mangle
                                                , sanitize
                                                )

cModuleFileNameBase :: ModuleName -> FilePath
cModuleFileNameBase (ModuleName modName) = toS $ sanitize modName

class GetCModuleFileName t where
  cModuleFileName :: ModuleName -> FilePath

instance GetCModuleFileName CModuleSource where
  cModuleFileName m = cModuleFileNameBase m <> ".c"

instance GetCModuleFileName CModuleHeader where
  cModuleFileName m = cModuleFileNameBase m <> ".h"

cItemName :: H.Module -> H.Item -> Text
cItemName modul item = cItemName' (modul ^. moduleName) iname
 where
  iname   = fromMaybe unnamed (item ^. H.itemName)
  unnamed = H.ItemName $ "$" <> (item ^. H.itemId . _Wrapped & show)

cImportedItemName :: H.ModuleName -> H.ItemName -> Text
cImportedItemName = cItemName'

cItemName' :: H.ModuleName -> H.ItemName -> Text
cItemName' modul item = mangle [modul ^. _Wrapped, item ^. _Wrapped]

cVarName :: H.Var -> String
cVarName var = var ^. H.varIdent . H.identName & sanitize & toS