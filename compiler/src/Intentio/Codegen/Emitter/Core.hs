{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Core
  ( emitCAssembly
  , emitCModuleHeader
  , emitCModuleSource
  , emitItemHeader
  , emitItemSource
  )
where

import           Intentio.Prelude        hiding ( op )

import           Control.Monad.Writer           ( tell )
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cexp
                                                , cinit
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

tyResult :: C.Type
tyResult = [cty| typename IeoResult |]

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
  return [cunit| $ty:tyResult $id:fname ($params:fparams) ; |]

emitFnItem :: H.Item -> H.BodyId -> MEmit [C.Definition]
emitFnItem item bodyId = do
  fname   <- getCItemName item
  body    <- getBodyById bodyId
  fparams <- withIB item body emitFnParams
  fbody   <- withIB item body emitFnBody
  return [cunit| $ty:tyResult $id:fname ($params:fparams) { $items:fbody } |]

emitFnParams :: BEmit [C.Param]
emitFnParams = view (_3 . H.bodyParams) >>= mapM emitFnParam

emitFnParam :: H.Param -> BEmit C.Param
emitFnParam param = do
  v <- cVarName <$> getParamVar param
  return [cparam| $ty:tyResult $id:v |]

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
emitFnVar var = return [citem| $ty:tyResult $id:v ; |] where v = cVarName var

emitFnBodyValue :: WriterT (Seq C.BlockItem) BEmit ()
emitFnBodyValue = view (_3 . H.bodyValue) >>= emitTopLevelExpr

--------------------------------------------------------------------------------
-- Expression emitter

emitTopLevelExpr :: H.Expr -> WriterT (Seq C.BlockItem) BEmit ()
emitTopLevelExpr expr = case expr ^. H.exprKind of
  H.PathExpr{}       -> wrap
  H.LitExpr{}        -> wrap
  H.UnExpr{}         -> wrap
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
  H.PathExpr path          -> emitPathExpr path
  H.LitExpr lit            -> emitLitExpr lit
  H.UnExpr op expr'        -> emitUnExpr op expr'
  H.BinExpr op lhs rhs     -> emitBinExpr op lhs rhs
  H.CallExpr callee args   -> emitCallExpr callee args
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

emitLitExpr :: H.Lit -> BEmit C.Exp
emitLitExpr lit = case lit ^. H.litKind of
  H.NoneLit      -> return [cexp| ieo_none |]
  H.IntegerLit x -> return [cexp| ieo_int_new( $llint:x ) |]
  H.FloatLit   x -> return [cexp| ieo_float_new( $ldouble:x ) |]
  H.CharLit    x -> return [cexp| ieo_char_new( $char:x ) |]
  H.StringLit  x -> let s = toS x
                    in return [cexp| ieo_string_new( $string:s ) |]
  H.RegexLit   x -> let s = toS x
                    in return [cexp| ieo_regex_new( $string:s ) |]

emitCallExpr :: H.Expr -> [H.Expr] -> BEmit C.Exp
emitCallExpr callee args = do
  ccallee <- emitExpr callee
  let arity = length args
  cargs <- fmap (\e -> [cinit| $e |]) <$> mapM emitExpr args
  let cargsArray = [cexp| ($ty:tyResult [$arity]){ $inits:cargs } |]
  return [cexp| ieo_call( $ccallee, $arity, $cargsArray) |]

emitAssignExpr :: H.VarId -> H.Expr -> BEmit C.Exp
emitAssignExpr varId expr = do
  vname <- cVarName <$> getVarById varId
  cexpr <- emitExpr expr
  let vid = [cexp| $id:vname |]
  return [cexp| $vid = $cexpr |]

emitUnExpr :: H.UnOp -> H.Expr -> BEmit C.Exp
emitUnExpr unOp expr = do
  f :: String <- case unOp ^. H.unOpKind of
    H.UnNot -> return "ieo_not"
    H.UnNeg -> return "ieo_neg"
  let cf = [cexp| $id:f |]
  cexpr <- emitExpr expr
  return [cexp| $cf ( $cexpr ) |]

emitBinExpr :: H.BinOp -> H.Expr -> H.Expr -> BEmit C.Exp
emitBinExpr binOp lhs rhs = do
  f :: String <- case binOp ^. H.binOpKind of
    H.BinAdd  -> return "ieo_add"
    H.BinDiv  -> return "ieo_div"
    H.BinEq   -> return "ieo_eq"
    H.BinGt   -> return "ieo_gt"
    H.BinGtEq -> return "ieo_gteq"
    H.BinLt   -> return "ieo_lt"
    H.BinLtEq -> return "ieo_lteq"
    H.BinMul  -> return "ieo_mul"
    H.BinNeq  -> return "ieo_neq"
    H.BinSEq  -> return "ieo_seq"
    H.BinSNeq -> return "ieo_sneq"
    H.BinSub  -> return "ieo_sub"
    H.BinAnd  -> lift $ pushIceFor binOp "HirImp: binary and operator"
    H.BinOr   -> lift $ pushIceFor binOp "HirImp: binary or operator"
    H.BinXor  -> lift $ pushIceFor binOp "HirImp: binary xor operator"
  let cf = [cexp| $id:f |]
  clhs <- emitExpr lhs
  crhs <- emitExpr rhs
  return [cexp| $cf ( $clhs , $crhs ) |]

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

  let includeIntentioH = [cunit| $esc:("#include <intentio.h>") |]

  let _cModuleDefDefinitions =  header
                             <> includeIntentioH
                             <> _cModuleDefDefinitions'

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
