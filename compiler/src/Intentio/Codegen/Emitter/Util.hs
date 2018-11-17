module Intentio.Codegen.Emitter.Util
  ( getItemById
  , getBodyById
  , getImpVarById
  , getMangledItemName
  , getParamVar
  , getImpParamVar
  )
where

import           Intentio.Prelude

import           Intentio.Codegen.Emitter.Monad ( MonadModuleEmit(..)
                                                , MonadBodyEmit(..)
                                                , MonadImpBodyEmit(..)
                                                )
import qualified Intentio.Codegen.Imp          as I
import           Intentio.Codegen.SymbolNames   ( cItemName )
import qualified Intentio.Hir                  as H

getItemById :: MonadModuleEmit m => H.ItemId -> m (H.Item ())
getItemById itemId = do
  modul <- askModule
  case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> fail $ "Bad HIR: missing item #" <> show itemId

getBodyById :: MonadModuleEmit m => H.BodyId -> m (H.Body ())
getBodyById bodyId = do
  modul <- askModule
  case modul ^? H.moduleBody bodyId of
    Just x  -> return x
    Nothing -> fail $ "Bad HIR: missing body #" <> show bodyId

getImpVarById :: MonadImpBodyEmit m => I.VarId -> m (I.Var ())
getImpVarById varId = do
  body <- askImpBody
  case body ^? I.bodyVar varId of
    Just x  -> return x
    Nothing -> fail $ "Bad Imp: missing variable #" <> show varId

getMangledItemName :: MonadModuleEmit m => H.Item () -> m String
getMangledItemName item = flip cItemName item <$> askModule

getParamVar :: MonadBodyEmit m => H.Param () -> m (H.Var ())
getParamVar param = do
  body <- askBody
  case H.findParamVar body param of
    Just v  -> return v
    Nothing -> fail $ "Bad HIR: missing parameter " <> show param

getImpParamVar :: MonadImpBodyEmit m => I.Param () -> m (I.Var ())
getImpParamVar param = do
  body <- askImpBody
  case I.findParamVar body param of
    Just v  -> return v
    Nothing -> fail $ "Bad Imp: missing parameter " <> show param
