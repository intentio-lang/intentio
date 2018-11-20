{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Body
  ( emitImpBody
  )
where

import           Intentio.Prelude

import qualified Data.ByteString               as BS
import qualified Data.List                     as List
import           Data.Loc                       ( noLoc )
import           Data.Scientific                ( toBoundedRealFloat )
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cexp
                                                , citem
                                                )

import           Intentio.Codegen.Emitter.Monad ( ImpBodyEmit
                                                , askAssembly
                                                , askImpBody
                                                )
import           Intentio.Codegen.Emitter.Util  ( getImpVarById )
import           Intentio.Codegen.SymbolNames   ( cItemName
                                                , cParamName
                                                , cVarName
                                                )
import qualified Intentio.Codegen.Imp          as I
import           Intentio.Compiler              ( resolveItem )

emitImpBody :: ImpBodyEmit [C.BlockItem]
emitImpBody = (<>) <$> emitVarDefs <*> emitBodyBlock

emitVarDefs :: ImpBodyEmit [C.BlockItem]
emitVarDefs = do
  body <- askImpBody
  let allVarIds      = body ^. I.bodyVarIds
  let paramVarIds    = body ^. I.bodyParams <&> view I.paramVarId
  let nonParamVarIds = allVarIds List.\\ paramVarIds
  (<>)
    <$> mapM (fmap (emitVarDef paramDef) . getImpVarById)      paramVarIds
    <*> mapM (fmap (emitVarDef uninitialized) . getImpVarById) nonParamVarIds
 where
  emitVarDef def var = [citem| typename IeoResult $id:v = $exp:d; |]
   where
    v = cVarName var
    d = def var

  paramDef var = [cexp| (typename IeoResult){ .succ = true, .term = $id:p } |]
    where p = cParamName var

  uninitialized (view I.varSucc -> True) = [cexp| ieo_none() |]
  uninitialized _                        = [cexp| ieo_none_fail() |]

emitBodyBlock :: ImpBodyEmit [C.BlockItem]
emitBodyBlock = askImpBody <&> view I.bodyBlock >>= emitBlock

emitBlock :: I.Block () -> ImpBodyEmit [C.BlockItem]
emitBlock = mapM emitStmt . view I.blockStmts

emitStmt :: I.Stmt () -> ImpBodyEmit C.BlockItem
emitStmt stmt = case stmt ^. I.stmtKind of
  I.ExprStmt   v e -> emitExprStmt v e
  I.AssignStmt d s -> emitAssignStmt d s
  I.WhileStmt  v w -> emitWhileStmt v w
  I.IfStmt c i e   -> emitIfStmt c i e
  I.ReturnStmt v   -> emitReturnStmt v

emitExprStmt :: I.VarId -> I.Expr () -> ImpBodyEmit C.BlockItem
emitExprStmt varId expr = do
  i <- emitVarById varId
  e <- emitExpr expr
  return [citem| $id:i = $exp:e; |]

emitAssignStmt :: I.VarId -> I.VarId -> ImpBodyEmit C.BlockItem
emitAssignStmt dst src = do
  d <- emitVarById dst
  s <- emitVarById src
  return [citem| $id:d = $id:s; |]

emitWhileStmt :: I.VarId -> I.Block () -> ImpBodyEmit C.BlockItem
emitWhileStmt = fail "Codegen for while statements is not implemented"

emitIfStmt :: I.VarId -> I.Block () -> I.Block () -> ImpBodyEmit C.BlockItem
emitIfStmt cond ifBlock elseBlock = do
  c  <- emitVarById cond
  ib <- emitBlock ifBlock
  eb <- emitBlock elseBlock
  return [citem| if (($id:c).succ) { $items:ib } else { $items:eb } |]

emitReturnStmt :: I.VarId -> ImpBodyEmit C.BlockItem
emitReturnStmt varId = do
  i <- emitVarById varId
  return [citem| return $id:i; |]

emitVarById :: I.VarId -> ImpBodyEmit C.Id
emitVarById = getImpVarById >=> emitVar

emitVar :: I.Var () -> ImpBodyEmit C.Id
emitVar var = return $ C.Id i noLoc where i = cVarName var

emitExpr :: I.Expr () -> ImpBodyEmit C.Exp
emitExpr expr = case expr ^. I.exprKind of
  I.VarExpr v -> do
    i <- emitVarById v
    return $ C.Var i noLoc

  I.BoxItemExpr _ _ -> fail "Codegen: Boxing items not implemented."

  I.LitExpr  lit    -> emitLitExpr lit

  I.SuccExpr v      -> do
    i <- idTerm <$> emitVarById v
    return [cexp| (typename IeoResult){ .succ = true, .term = $exp:i } |]

  I.FailExpr v -> do
    i <- idTerm <$> emitVarById v
    return [cexp| (typename IeoResult){ .succ = false, .term = $exp:i } |]

  I.NotExpr v -> do
    i <- emitVarById v
    let s = idSucc i
    let t = idTerm i
    return [cexp| (typename IeoResult){ .succ = $exp:s, .term = $exp:t } |]

  I.UnExpr o v -> do
    let (f :: String) = case o of
          I.UnNeg -> "ieo_neg"
    i <- idTerm <$> emitVarById v
    return [cexp| $id:f ( $exp:i ) |]

  I.BinExpr o l r -> do
    let (f :: String) = case o of
          I.BinAdd  -> "ieo_add"
          I.BinDiv  -> "ieo_div"
          I.BinEq   -> "ieo_eq"
          I.BinGt   -> "ieo_gt"
          I.BinGtEq -> "ieo_gteq"
          I.BinLt   -> "ieo_lt"
          I.BinLtEq -> "ieo_lteq"
          I.BinMul  -> "ieo_mul"
          I.BinNeq  -> "ieo_neq"
          I.BinSEq  -> "ieo_seq"
          I.BinSNeq -> "ieo_sneq"
          I.BinSub  -> "ieo_sub"
    li <- idTerm <$> emitVarById l
    ri <- idTerm <$> emitVarById r
    return [cexp| $id:f ( $exp:li , $exp:ri ) |]

  I.CallStaticExpr mName iName args -> do
    (modul, item) <- askAssembly >>= lift . resolveItem mName iName
    let f = flip C.Var noLoc $ C.Id (cItemName modul item) noLoc
    cargs <- fmap idTerm <$> mapM emitVarById args
    return $ C.FnCall f cargs noLoc

  I.CallDynamicExpr _ _ -> fail "Codegen: dynamic CallExpr not implemented."

emitLitExpr :: I.Lit () -> ImpBodyEmit C.Exp
emitLitExpr lit = case lit ^. I.litKind of
  I.NoneLit      -> return [cexp| ieo_none |]
  I.IntegerLit x -> return [cexp| ieo_int_new( $llint:x ) |]
  I.FloatLit   x -> case toBoundedRealFloat x of
    Left _ ->
      fail
        $  "Float literal '"
        <> show x
        <> "' is to big for safe code generation."
    Right f -> return [cexp| ieo_float_new( $ldouble:f ) |]
  I.StringLit x -> return [cexp| ieo_string_new( $string:s , $llint:n ) |]
   where
    s = toS x
    n = BS.length (toS x)

idSucc :: C.Id -> C.Exp
idSucc i = [cexp| ($id:i).succ |]

idTerm :: C.Id -> C.Exp
idTerm i = [cexp| ($id:i).term |]
