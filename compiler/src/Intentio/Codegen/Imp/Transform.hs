module Intentio.Codegen.Imp.Transform
  ( impTransform
  )
where

import           Intentio.Prelude

import qualified Intentio.Codegen.Imp.Model    as I
import           Intentio.Codegen.SymbolNames   ( cTmpVarName )
import           Intentio.Compiler              ( CompilePure
                                                , pushIceFor
                                                )
import           Intentio.Diagnostics           ( sourcePos )
import qualified Intentio.Hir                  as H

--------------------------------------------------------------------------------
-- Imp monad

data ImpState = ImpState
  { _currentStmts  :: [I.Stmt ()]
  , _impVars       :: [I.Var ()]
  , _impVarCounter :: I.VarId
  }

type ImpM a = StateT ImpState CompilePure a

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''ImpState

--------------------------------------------------------------------------------
-- Imp monad utilities

emptyImpState :: ImpState
emptyImpState =
  ImpState { _currentStmts = [], _impVars = [], _impVarCounter = toEnum 0 }

withBlock :: ImpM a -> ImpM (I.Block ())
withBlock f = do
  prevStmts <- currentStmts <<.= []
  void f
  stmts <- currentStmts <<.= prevStmts
  return $ I.Block () (reverse stmts)

pushStmt :: I.StmtKind () -> ImpM ()
pushStmt s = currentStmts %= cons (I.Stmt () s)

pushExpr' :: I.VarId -> I.ExprKind () -> ImpM ()
pushExpr' v e = pushStmt (I.ExprStmt v (I.Expr () e))

pushExpr :: I.ExprKind () -> ImpM I.VarId
pushExpr e = do
  v <- allocVar
  pushExpr' v e
  return v

allocVar :: ImpM I.VarId
allocVar = do
  _varId <- impVarCounter <<%= succ
  let _varAnn       = ()
  let _varSourcePos = () ^. sourcePos
  let _varName      = cTmpVarName _varId
  impVars %= cons I.Var { .. }
  return _varId

pattern DirectPath :: H.PathKind a -> H.Expr a
-- brittany-disable-next-binding
pattern DirectPath pk <- (preview (H.exprKind . H._PathExpr . H.pathKind)
                          -> Just pk)

--------------------------------------------------------------------------------
-- Imp transformation code

impTransform :: H.Body () -> CompilePure (I.Body ())
impTransform b = do
  (_bodyBlock, s) <- runStateT (impExprToBlock $ b ^. H.bodyValue) emptyImpState
  let _bodyAnn    = ()
  let _bodyParams = b ^. H.bodyParams
  let _bodyVars = foldl' (\m v -> m & at (v ^. I.varId . I.unVarId) ?~ v)
                         (b ^. H.bodyVars)
                         (s ^. impVars)
  let _bodyVarIds = (b ^. H.bodyVarIds) <> (s ^. impVars <&> view I.varId)
  return I.Body { .. }

impExprToBlock :: H.Expr () -> ImpM (I.Block ())
impExprToBlock H.Expr { _exprKind = H.BlockExpr b } = impBlock b
impExprToBlock expr = withBlock $ impExpr expr

impBlock :: H.Block () -> ImpM (I.Block ())
impBlock = withBlock . mapM_ impExpr . view H.blockExprs

impExpr :: H.Expr () -> ImpM I.VarId
impExpr expr' = case expr' ^. H.exprKind of
  H.PathExpr p' -> case p' ^. H.pathKind of
    H.ToVar v    -> return v
    H.ToItem _ _ -> lift $ pushIceFor p' "Imp: Boxing items not implemented."

  H.LitExpr   l -> pushExpr $ I.LitExpr l

  H.BlockExpr _ -> lift $ pushIceFor expr' "Imp: BlockExpr not implemented."

  H.SuccExpr  e -> I.SuccExpr <$> impExpr e >>= pushExpr

  H.FailExpr  e -> I.FailExpr <$> impExpr e >>= pushExpr

  H.UnExpr o' e -> case o' ^. H.unOpKind of
    H.UnNeg -> go I.UnNeg
    H.UnNot -> I.NotExpr <$> impExpr e >>= pushExpr
    where go o = I.UnExpr o <$> impExpr e >>= pushExpr

  H.BinExpr o' l r -> case o' ^. H.binOpKind of
    H.BinAdd  -> go I.BinAdd
    H.BinDiv  -> go I.BinDiv
    H.BinEq   -> go I.BinEq
    H.BinGt   -> go I.BinGt
    H.BinGtEq -> go I.BinGtEq
    H.BinLt   -> go I.BinLt
    H.BinLtEq -> go I.BinLtEq
    H.BinMul  -> go I.BinMul
    H.BinNeq  -> go I.BinNeq
    H.BinSEq  -> go I.BinSEq
    H.BinSNeq -> go I.BinSNeq
    H.BinSub  -> go I.BinSub
    H.BinXor  -> go I.BinXor
    H.BinAnd  -> lift $ pushIceFor o' "Imp: 'and' not implemented."
    H.BinOr   -> lift $ pushIceFor o' "Imp: 'or' not implemented."
    where go o = I.BinExpr o <$> impExpr l <*> impExpr r >>= pushExpr

  -- Optimization: calls directly to items do not require item boxing.
  H.CallExpr (DirectPath (H.ToItem mName iName)) args -> do
    r <- allocVar
    let proc [] vs = pushExpr' r $ I.CallStaticExpr mName iName (reverse vs)
        proc (a : as) vs = do
          v  <- impExpr a
          ib <- withBlock $ proc as (v : vs)
          eb <- withBlock . pushStmt $ I.AssignStmt r v
          pushStmt $ I.IfStmt v ib eb
    proc args []
    return r

  H.CallExpr _ _ ->
    lift $ pushIceFor expr' "Imp: dynamic CallExpr not implemented."

  H.WhileExpr _ _  -> lift $ pushIceFor expr' "Imp: WhileExpr not implemented."

  H.IfExpr _ _ _   -> lift $ pushIceFor expr' "Imp: IfExpr not implemented."

  H.AssignExpr v e -> I.AssignStmt v <$> impExpr e >>= pushStmt >> return v

  H.ReturnExpr e   -> do
    v <- impExpr e
    pushStmt $ I.ReturnStmt v
    return v
