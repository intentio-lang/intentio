{-# LANGUAGE QuasiQuotes #-}

module Intentio.Codegen.Emitter.Body
  ( emitImpBody
  )
where

import           Intentio.Prelude

import qualified Data.List                     as List
import           Data.Loc                       ( noLoc )
import qualified Language.C.Quote              as C
import           Language.C.Quote.C             ( citem )

import           Intentio.Codegen.Emitter.Monad ( ImpBodyEmit
                                                , MonadImpBodyEmit
                                                , WT
                                                , askImpBody
                                                , execWT
                                                , tellWT
                                                )
import           Intentio.Codegen.Emitter.Util  ( tyIeoResult
                                                , getImpVarById
                                                )
import           Intentio.Codegen.SymbolNames   ( cVarName )
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

emitVarIdById :: MonadImpBodyEmit m => I.VarId -> m C.Id
emitVarIdById = getImpVarById >=> emitVarId

emitVarId :: MonadImpBodyEmit m => I.Var () -> m C.Id
emitVarId var = return $ C.Id i noLoc where i = var ^. I.varName & toS

emitExpr :: MonadImpBodyEmit m => I.Expr () -> m C.Exp
emitExpr = fail "Codegen for expressions is not implemented"
