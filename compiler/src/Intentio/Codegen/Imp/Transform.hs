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
  { _currentStmts   :: [I.Stmt ()]
  , _impVars        :: [I.Var ()]
  , _firstFreeVarId :: I.VarId
  }

type ImpM a = StateT ImpState CompilePure a

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''ImpState

--------------------------------------------------------------------------------
-- Imp monad utilities

emptyImpState :: I.VarId -> ImpState
emptyImpState _firstFreeVarId =
  ImpState { _currentStmts = [], _impVars = [], _firstFreeVarId }

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
allocVar = allocVar' False

allocVar' :: Bool -> ImpM I.VarId
allocVar' _varSucc = do
  _varId <- preuses (impVars . _head . I.varId) succ >>= \case
    Just i  -> return i
    Nothing -> use firstFreeVarId
  let _varAnn       = ()
  let _varSourcePos = () ^. sourcePos
  let _varName      = cTmpVarName _varId
  impVars %= cons I.Var { .. }
  return _varId

emptyBlock :: I.Block ()
emptyBlock = I.Block () []

pattern DirectPath :: H.PathKind a -> H.Expr a
-- brittany-disable-next-binding
pattern DirectPath pk <- (preview (H.exprKind . H._PathExpr . H.pathKind)
                          -> Just pk)

--------------------------------------------------------------------------------
-- Imp transformation code

impTransform :: H.Body () -> CompilePure (I.Body ())
impTransform b = do
  let st = emptyImpState getFirstFreeVarId
  (_bodyBlock, s) <- runStateT (impBodyValue $ b ^. H.bodyValue) st
  let _bodyAnn    = ()
  let _bodyParams = b ^. H.bodyParams
  let _bodyVars = foldl' addVar (b ^. H.bodyVars <&> impVar) (s ^. impVars)
  let _bodyVarIds =
        (b ^. H.bodyVarIds) <> (s ^. impVars <&> view I.varId & reverse)
  return I.Body { .. }
 where
  addVar m v = m & at (v ^. I.varId . I.unVarId) ?~ v

  getFirstFreeVarId = case b ^. H.bodyVarIds of
    [] -> I.VarId 0
    vs -> succ . maximum $ vs

impVar :: H.Var () -> I.Var ()
impVar H.Var { _varAnn, _varId, _varName } = I.Var { _varSucc = False, .. }

impBodyValue :: H.Expr () -> ImpM (I.Block ())
impBodyValue expr = withBlock $ do
  r <- impExpr expr
  pushStmt $ I.ReturnStmt r

impExpr :: H.Expr () -> ImpM I.VarId
impExpr expr' = case expr' ^. H.exprKind of
  H.PathExpr p' -> case p' ^. H.pathKind of
    H.ToVar v    -> return v
    H.ToItem _ _ -> lift $ pushIceFor p' "Imp: Boxing items not implemented."

  H.LitExpr   l -> pushExpr $ I.LitExpr l

  H.BlockExpr b -> proc $ b ^. H.blockExprs
   where
    proc []       = allocVar' True
    proc [e     ] = impExpr e
    proc (e : es) = impExpr e >> proc es

  H.SuccExpr e  -> I.SuccExpr <$> impExpr e >>= pushExpr

  H.FailExpr e  -> I.FailExpr <$> impExpr e >>= pushExpr

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

    H.BinAnd  -> do
      rs <- allocVar
      a  <- impExpr l
      ib <- withBlock $ do
        b <- impExpr r
        pushStmt $ I.AssignStmt rs b
      eb <- withBlock . pushStmt $ I.AssignStmt rs a
      pushStmt $ I.IfStmt a ib eb
      return rs

    H.BinOr -> do
      rs <- allocVar
      a  <- impExpr l
      ib <- withBlock . pushStmt $ I.AssignStmt rs a
      eb <- withBlock $ do
        b <- impExpr r
        pushStmt $ I.AssignStmt rs b
      pushStmt $ I.IfStmt a ib eb
      return rs

    H.BinXor -> lift $ pushIceFor o' "Imp: 'xor' not implemented."
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

  H.WhileExpr cond body -> do
    r  <- allocVar
    c  <- allocVar
    cb <- withBlock $ impExpr cond >>= pushStmt . I.AssignStmt c
    bb <- withBlock $ impExpr body >>= pushStmt . I.AssignStmt r
    pushStmt $ I.WhileStmt cb c bb
    return r

  H.IfExpr cond ifBlock elseBlockOpt -> do
    r  <- allocVar
    vc <- impExpr cond
    ib <- withBlock $ impExpr ifBlock >>= pushStmt . I.AssignStmt r
    eb <- case elseBlockOpt of
      Just elseBlock ->
        withBlock $ impExpr elseBlock >>= pushStmt . I.AssignStmt r
      Nothing -> return emptyBlock
    pushStmt $ I.IfStmt vc ib eb
    return r

  H.AssignExpr v e -> I.AssignStmt v <$> impExpr e >>= pushStmt >> return v

  H.ReturnExpr e   -> do
    v <- impExpr e
    pushStmt $ I.ReturnStmt v
    return v
