{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Core
  ( emitCAssembly
  , emitCModuleHeader
  , emitCModuleSource
  , emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude

import           Control.Monad.Writer           ( tell )
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cexp
                                                , citem
                                                , citems
                                                , cparam
                                                , cunit
                                                , cty
                                                )
import           NeatInterpolation              ( text )

import           Intentio.Compiler              ( ModuleName(..)
                                                , Assembly
                                                , assemblyMainModuleName
                                                , concatMapModulesM
                                                , CompilePure
                                                , pushIceFor
                                                )
import qualified Intentio.Hir                  as H

import           Intentio.Codegen.Emitter.Types ( CModuleHeader
                                                , CModuleSource
                                                , CModuleDef(..)
                                                , cModuleEraseType
                                                )
import           Intentio.Codegen.SymbolNames   ( GetCModuleFileName(..)
                                                , cItemName
                                                , cImportedItemName
                                                , cVarName
                                                )

--------------------------------------------------------------------------------
-- Emitter monad

type Emit r = ReaderT r CompilePure
type MEmit = Emit H.Module
type IEmit = Emit (H.Module, H.Item)
type BEmit = Emit (H.Module, H.Item, H.Body)

withI :: H.Item -> IEmit a -> MEmit a
withI i = withReaderT (, i)

withB :: H.Body -> BEmit a -> IEmit a
withB b = withReaderT $ \(m, i) -> (m, i, b)

withIB :: H.Item -> H.Body -> BEmit a -> MEmit a
withIB i b = withReaderT (, i, b)

unI :: MEmit a -> IEmit a
unI = withReaderT $ view _1

unB :: IEmit a -> BEmit a
unB = withReaderT $ \(m, i, _) -> (m, i)

unIB :: MEmit a -> BEmit a
unIB = withReaderT $ view _1

--------------------------------------------------------------------------------
-- Emitter entry points

emitCAssembly :: Assembly H.Module -> CompilePure (Assembly (CModuleDef Void))
emitCAssembly = fmap addMainName . concatMapModulesM emit
 where
  addMainName asm = asm & (assemblyMainModuleName %~ fmap mkMainName)

  mkMainName = ModuleName . toS . cModuleFileName @CModuleSource

  emit modul = do
    header <- cModuleEraseType <$> emitCModuleHeader modul
    source <- cModuleEraseType <$> emitCModuleSource modul
    return [header, source]

emitCModuleHeader :: H.Module -> CompilePure (CModuleDef CModuleHeader)
emitCModuleHeader = runReaderT (emitCModule' emitItemHeader')

emitCModuleSource :: H.Module -> CompilePure (CModuleDef CModuleSource)
emitCModuleSource = runReaderT (emitCModule' emitItemSource')

emitItemHeader :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemHeader modul itemId = runReaderT (emitItemHeader' itemId) modul

emitItemSource :: H.Module -> H.ItemId -> CompilePure [C.Definition]
emitItemSource modul itemId = runReaderT (emitItemSource' itemId) modul

--------------------------------------------------------------------------------
-- Constants

iobjTy :: C.Type
iobjTy = [cty| typename IeoObject |]

iobjPtr :: C.Type
iobjPtr = [cty| $ty:iobjTy * |]

--------------------------------------------------------------------------------
-- Main item emitter

emitItemHeader' :: H.ItemId -> MEmit [C.Definition]
emitItemHeader' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem _ _ -> return []
    H.FnItem bodyId  -> emitFnHeader item bodyId

emitItemSource' :: H.ItemId -> MEmit [C.Definition]
emitItemSource' itemId = do
  item <- getItemById itemId
  case item ^. H.itemKind of
    H.ImportItem modName _ -> emitImportItem modName
    H.FnItem bodyId        -> emitFnItem item bodyId

emitImportItem :: ModuleName -> MEmit [C.Definition]
emitImportItem modName = return [cunit| $esc:f |]
  where f = "#include \"" <> cModuleFileName @CModuleHeader modName <> "\""

--------------------------------------------------------------------------------
-- Function declaration emitter

emitFnHeader :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnHeader item bodyId = do
  fname   <- getCItemName item
  body    <- getBodyById bodyId
  fparams <- withIB item body emitFnParams
  return [cunit| $ty:iobjPtr $id:fname ($params:fparams) ; |]

emitFnItem :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnItem item bodyId = do
  fname   <- getCItemName item
  body    <- getBodyById bodyId
  fparams <- withIB item body emitFnParams
  fbody   <- withIB item body emitFnBody
  return [cunit| $ty:iobjPtr $id:fname ($params:fparams) { $items:fbody } |]

emitFnParams :: BEmit [C.Param]
emitFnParams = view (_3 . H.bodyParams) >>= mapM emitFnParam

emitFnParam :: H.Param -> BEmit C.Param
emitFnParam param = do
  v <- cVarName <$> getParamVar param
  return [cparam| $ty:iobjPtr $id:v |]

emitFnBody :: BEmit [C.BlockItem]
emitFnBody = toList <$> execWriterT (emitFnVars >> emitFnBodyValue)

emitFnVars :: WriterT (Seq C.BlockItem) BEmit ()
emitFnVars = do
  body <- view _3
  let paramVarIds    = body ^. H.bodyParams <&> view _Wrapped
  let allVarIds      = body ^. H.bodyVarIds
  let nonParamVarIds = allVarIds List.\\ paramVarIds
  forM_ nonParamVarIds
    $ \i -> lift (getVarById i) >>= lift . emitFnVar >>= tell . pure

emitFnVar :: H.Var -> BEmit C.BlockItem
emitFnVar var = return [citem| $ty:iobjPtr $id:v ; |] where v = cVarName var

emitFnBodyValue :: WriterT (Seq C.BlockItem) BEmit ()
emitFnBodyValue = view (_3 . H.bodyValue) >>= emitTopLevelExpr

--------------------------------------------------------------------------------
-- Expression emitter

emitTopLevelExpr :: H.Expr -> WriterT (Seq C.BlockItem) BEmit ()
emitTopLevelExpr expr = case expr ^. H.exprKind of
  H.PathExpr{}       -> wrap
  H.LiteralExpr{}    -> wrap
  H.UnaryExpr{}      -> wrap
  H.BinExpr{}        -> wrap
  H.CallExpr{}       -> wrap
  H.AssignExpr{}     -> wrap

  H.BlockExpr block  -> mapM_ emitTopLevelExpr (block ^. H.blockExprs)

  H.WhileExpr _ _    -> lift . lift $ pushIceFor expr "TODO: while expr"
  H.IfExpr _ _ _     -> lift . lift $ pushIceFor expr "TODO: if expr"

  H.ReturnExpr inner -> do
    e <- lift $ emitExpr inner
    tell $ fromList [citems| return $e ; |]
 where
  wrap = do
    e <- lift $ emitExpr expr
    tell $ fromList [citems| $e; |]

emitExpr :: H.Expr -> BEmit C.Exp
emitExpr expr = case expr ^. H.exprKind of
  H.PathExpr    path -> emitPathExpr path

  H.LiteralExpr _    -> lift $ pushIceFor expr "TODO: literal expr"
  H.UnaryExpr _ _    -> lift $ pushIceFor expr "TODO: unary expr"
  H.BinExpr _ _ _    -> lift $ pushIceFor expr "TODO: binary expr"
  H.CallExpr   _ _   -> lift $ pushIceFor expr "TODO: call expr"

  H.AssignExpr varId inner -> emitAssignExpr varId inner

  H.BlockExpr{}  -> lift $ pushIceFor expr "HirImp Bug: block in inner"
  H.WhileExpr{}  -> lift $ pushIceFor expr "HirImp Bug: while in inner"
  H.IfExpr{}     -> lift $ pushIceFor expr "HirImp Bug: if in inner"
  H.ReturnExpr{} -> lift $ pushIceFor expr "HirImp Bug: return in inner"

emitPathExpr :: H.Path -> BEmit C.Exp
emitPathExpr path = case path ^. H.pathKind of
  H.Local  varId  -> getVarById varId <&> cVarName <&> mkExp
  H.Global itemId -> unIB (getItemById itemId >>= getCItemName) <&> mkExp
  where mkExp v = [cexp| $id:v |]

emitAssignExpr :: H.VarId -> H.Expr -> BEmit C.Exp
emitAssignExpr varId expr = do
  vname <- cVarName <$> getVarById varId
  cexpr <- emitExpr expr
  let vid = [cexp| $id:vname |]
  return [cexp| $vid = $cexpr |]

--------------------------------------------------------------------------------
-- Helpers

emitCModule'
  :: forall t
   . GetCModuleFileName t
  => (H.ItemId -> MEmit [C.Definition])
  -> MEmit (CModuleDef t)
emitCModule' f = do
  _cModuleDefSourcePos    <- view H.moduleSourcePos
  _cModuleDefIntentioName <- view H.moduleName
  _cModuleDefFileName     <- cModuleFileName @t <$> view H.moduleName
  _cModuleDefDefinitions' <- view H.moduleItemIds >>= mapM f <&> concat

  let headerLine       = T.replicate 77 "-"
  let headerFileName   = toS _cModuleDefFileName
  let headerModuleName = _cModuleDefIntentioName ^. _Wrapped

  let headerText = toS [text|
      //$headerLine
      // Generated by intentioc. DO NOT MODIFY!
      // File:   $headerFileName
      // Module: $headerModuleName
      //$headerLine
    |]

  let header = [cunit| $esc:headerText |]

  let _cModuleDefDefinitions = header <> _cModuleDefDefinitions'

  return CModuleDef {..}


getItemById :: H.ItemId -> MEmit H.Item
getItemById itemId = do
  modul <- ask
  case modul ^? H.moduleItem itemId of
    Just x  -> return x
    Nothing -> lift $ pushIceFor modul $ "Bad HIR: miss item " <> show itemId

getBodyById :: H.BodyId -> MEmit H.Body
getBodyById bodyId = do
  modul <- ask
  case modul ^? H.moduleBody bodyId of
    Just x  -> return x
    Nothing -> lift $ pushIceFor modul $ "Bad HIR: miss body " <> show bodyId

getVarById :: H.VarId -> BEmit H.Var
getVarById varId = do
  body <- view _3
  case body ^? H.bodyVar varId of
    Just x  -> return x
    Nothing -> lift $ pushIceFor body $ "Bad HIR: miss var " <> show varId

getCItemName :: H.Item -> MEmit String
getCItemName item = case item ^. H.itemKind of
  H.ImportItem m i -> cImportedItemName m i & toS & return
  _                -> ask <&> flip cItemName item <&> toS

getParamVar :: H.Param -> BEmit H.Var
getParamVar param = do
  item <- view _2
  body <- view _3
  case H.findParamVar body param of
    Just v  -> return v
    Nothing -> lift $ pushIceFor item $ "Bad HIR: miss param " <> show param
