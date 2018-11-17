{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Item
  ( emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude

import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cparam
                                                , cunit
                                                )

import           Intentio.Codegen.Emitter.Body  ( emitImpBody )
import           Intentio.Codegen.Emitter.Monad ( ItemEmit
                                                , BodyEmit
                                                , runBodyEmit
                                                , runImpBodyEmit
                                                , askItem
                                                , askBody
                                                )
import           Intentio.Codegen.Emitter.Util  ( getBodyById
                                                , getMangledItemName
                                                , getParamVar
                                                )
import           Intentio.Codegen.SymbolNames   ( cVarName )
import qualified Intentio.Codegen.Imp          as I
import qualified Intentio.Hir                  as H

--------------------------------------------------------------------------------
-- Entry points

emitItemHeader :: ItemEmit [C.Definition]
emitItemHeader = do
  item <- askItem
  case item ^. H.itemKind of
    H.FnItem bodyId -> emitFnHeader bodyId

emitItemSource :: ItemEmit [C.Definition]
emitItemSource = do
  item <- askItem
  case item ^. H.itemKind of
    H.FnItem bodyId -> emitFnSource bodyId

--------------------------------------------------------------------------------
-- Function item emitter

emitFnHeader :: H.BodyId -> ItemEmit [C.Definition]
emitFnHeader bodyId = do
  item <- askItem
  body <- getBodyById bodyId
  f    <- getMangledItemName item
  pars <- runBodyEmit emitFnParams body
  return [cunit| typename IeoResult $id:f ($params:pars) ; |]

emitFnParams :: BodyEmit [C.Param]
emitFnParams = (view H.bodyParams <$> askBody) >>= mapM emitFnParam

emitFnParam :: H.Param () -> BodyEmit C.Param
emitFnParam param = do
  v <- cVarName <$> getParamVar param
  return [cparam| typename IeoTerm * $id:v |]

emitFnSource :: H.BodyId -> ItemEmit [C.Definition]
emitFnSource bodyId = do
  item    <- askItem
  f       <- getMangledItemName item
  body    <- getBodyById bodyId
  impBody <- lift $ I.impTransform body
  pars    <- runBodyEmit emitFnParams body
  cbody   <- runImpBodyEmit emitImpBody impBody
  return [cunit| typename IeoResult $id:f ($params:pars) { $items:cbody } |]
