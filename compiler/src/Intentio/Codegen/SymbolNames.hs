{-# LANGUAGE AllowAmbiguousTypes #-}

module Intentio.Codegen.SymbolNames
  ( GetCModuleFileName(..)
  , cModuleFileNameBase
  , cItemName
  , cItemName'
  , cVarName
  , cTmpVarName
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
                                                )

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

cVarName :: (Eq a, Show a) => H.Var a -> String
cVarName var = toS . mangle $ [var ^. H.varName]

cTmpVarName :: H.VarId -> Text
cTmpVarName i = '%' <| show (i ^. H.unVarId)
