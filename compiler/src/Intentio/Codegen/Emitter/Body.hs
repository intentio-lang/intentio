{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Body
  ( emitImpBody
  )
where

import           Intentio.Prelude

import qualified Data.List                     as List
import           Data.Loc                       ( noLoc )
import           Data.Scientific                ( toBoundedRealFloat )
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( cexp
                                                , citem
                                                )

import           Intentio.Codegen.Emitter.Monad ( ImpBodyEmit
                                                , WT
                                                , askImpBody
                                                , execWT
                                                , tellWT
                                                )
import           Intentio.Codegen.Emitter.Util  ( tyIeoResult
                                                , getImpVarById
                                                )
import           Intentio.Codegen.SymbolNames   ( cItemName'
                                                , cVarName
                                                )
import qualified Intentio.Codegen.Imp          as I

type W a = WT C.BlockItem ImpBodyEmit a

emitImpBody :: ImpBodyEmit [C.BlockItem]
emitImpBody = execWT (emitVars >> emitBodyBlock)

emitVars :: W ()
emitVars = do
  body <- askImpBody
  let allVarIds      = body ^. I.bodyVarIds
  let paramVarIds    = body ^. I.bodyParams <&> view I.paramVarId
  let nonParamVarIds = allVarIds List.\\ paramVarIds
  forM_ nonParamVarIds $ getImpVarById >=> emitVar

emitVar :: I.Var () -> W ()
emitVar var = tellWT [citem| $ty:tyIeoResult $id:v; |] where v = cVarName var

emitBodyBlock :: W ()
emitBodyBlock = askImpBody <&> view I.bodyBlock >>= emitBlock

emitBlock :: I.Block () -> W ()
emitBlock = mapM_ emitStmt . view I.blockStmts

emitStmt :: I.Stmt () -> W ()
emitStmt stmt = case stmt ^. I.stmtKind of
  I.ExprStmt   v e -> emitExprStmt v e
  I.AssignStmt d s -> emitAssignStmt d s
  I.WhileStmt  v w -> emitWhileStmt v w
  I.IfStmt c i e   -> emitIfStmt c i e
  I.ReturnStmt v   -> emitReturnStmt v

emitExprStmt :: I.VarId -> I.Expr () -> W ()
emitExprStmt varId expr = do
  i <- emitVarIdById varId
  e <- emitExpr expr
  tellWT [citem| $id:i = $exp:e; |]

emitAssignStmt :: I.VarId -> I.VarId -> W ()
emitAssignStmt dst src = do
  d <- emitVarIdById dst
  s <- emitVarIdById src
  tellWT [citem| $id:d = $id:s; |]

emitWhileStmt :: I.VarId -> I.Block () -> W ()
emitWhileStmt = fail "Codegen for while statements is not implemented"

emitIfStmt :: I.VarId -> I.Block () -> I.Block () -> W ()
emitIfStmt = fail "Codegen for if statements is not implemented"

emitReturnStmt :: I.VarId -> W ()
emitReturnStmt varId = do
  i <- emitVarIdById varId
  tellWT [citem| return $id:i; |]

emitVarIdById :: I.VarId -> W C.Id
emitVarIdById = getImpVarById >=> emitVarId

emitVarId :: I.Var () -> W C.Id
emitVarId var = return $ C.Id i noLoc where i = var ^. I.varName & toS

emitExpr :: I.Expr () -> W C.Exp
emitExpr expr = case expr ^. I.exprKind of
  I.VarExpr v -> do
    i <- emitVarIdById v
    return $ C.Var i noLoc

  I.BoxItemExpr _ _ -> fail "Codegen: Boxing items not implemented."

  I.LitExpr  lit    -> emitLitExpr lit

  I.SuccExpr v      -> do
    i <- emitVarIdById v
    return [cexp| ($ty:tyIeoResult){ .succ = true, .term = ($id:i).term } |]

  I.FailExpr v -> do
    i <- emitVarIdById v
    return [cexp| ($ty:tyIeoResult){ .succ = false, .term = ($id:i).term } |]

  I.NotExpr v -> do
    i <- emitVarIdById v
    let s = [cexp| !($id:i).succ |]
    let t = [cexp| ($id:i).term |]
    return [cexp| ($ty:tyIeoResult){ .succ = $exp:s, .term = $exp:t } |]

  I.UnExpr o v -> do
    let (f :: String) = case o of
          I.UnNeg -> "ieo_neg"
    i <- emitVarIdById v
    return [cexp| $id:f ( $id:i ) |]

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
    li <- emitVarIdById l
    ri <- emitVarIdById r
    return [cexp| $id:f ( $id:li , $id:ri ) |]

  I.CallStaticExpr mName iName args -> do
    let f = flip C.Var noLoc $ C.Id (cItemName' mName iName) noLoc
    cargs <- fmap (flip C.Var noLoc) <$> mapM emitVarIdById args
    return $ C.FnCall f cargs noLoc

  I.CallDynamicExpr _ _ -> fail "Codegen: dynamic CallExpr not implemented."

emitLitExpr :: I.Lit () -> W C.Exp
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
  I.StringLit x -> return [cexp| ieo_string_new( $string:s ) |]
    where s = toS x
