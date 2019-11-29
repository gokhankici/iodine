{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.SSA
  ( ssa
  , SSAOutput
  , SSAIR
  )
where

import           Iodine.Language.IRParser       ( ParsedIR )
import           Iodine.Language.IR
import           Iodine.Types

import           Control.Lens
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as SQ

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Trace

type FD r      = Members '[State St, Trace] r
type FDM r     = (FD r, Members '[Reader ModuleName] r)
type FDM2 r    = (FDM r, Members '[State Vars] r)
type SSAIR     = L (Module Int)
type SSAOutput = (SSAIR, TRNextVars)

newtype ModuleName = ModuleName { getModuleName :: Id }

-- -----------------------------------------------------------------------------
-- after this step, each new variable, statement, and always block
-- within a module will have an unique id
ssa :: Member Trace r => ParsedIR -> Sem r SSAOutput
-- -----------------------------------------------------------------------------
ssa modules = traverse ssaModule modules <* trace "SSA DONE!" & runSSA

-- -----------------------------------------------------------------------------
-- run ssa on all the statements inside the given module
ssaModule :: FD r => Module a -> Sem r (Module Int)
-- -----------------------------------------------------------------------------
ssaModule Module {..} =
  Module moduleName ports variables
  <$> traverse ssaStmtSingle gateStmts
  <*> traverse ssaAB         alwaysBlocks
  <*> traverse ssaModuleInstance moduleInstances
  <*> freshId ModId
  & runReader (ModuleName moduleName)

-- -----------------------------------------------------------------------------
-- run ssa on all the statements inside the given always block
ssaAB :: FDM r => AlwaysBlock a -> Sem r (AlwaysBlock Int)
-- -----------------------------------------------------------------------------
ssaAB ab@AlwaysBlock {..} =
  withAB ab
    $   AlwaysBlock
    <$> ssaEvent abEvent
    <*> ssaStmtSingle abStmt
    <*> freshId ABId

ssaEvent :: FD r => Event a -> Sem r (Event Int)
ssaEvent PosEdge {..} = PosEdge <$> ssaExpr eventExpr <*> freshId NoId
ssaEvent NegEdge {..} = NegEdge <$> ssaExpr eventExpr <*> freshId NoId
ssaEvent Star {..}    = Star <$> freshId NoId

-- -----------------------------------------------------------------------------
ssaStmtSingle :: FDM r => Stmt a -> Sem r (Stmt Int)
-- -----------------------------------------------------------------------------
ssaStmtSingle s = ssaStmt s & evalState @Vars mempty & withStmt

ssaStmt :: FDM2 r => Stmt a -> Sem r (Stmt Int)
ssaStmt Block {..} = Block <$> traverse ssaStmt blockStmts <*> freshId StmtId
ssaStmt Assignment {..} = case assignmentType of
  Continuous  -> ssaContinousAssignment assignmentLhs assignmentRhs
  Blocking    -> ssaBlockingAssignment assignmentLhs assignmentRhs
  NonBlocking -> do
    modify (HS.insert $ varName assignmentLhs)
    ssaNonBlockingAssignment assignmentLhs assignmentRhs
ssaStmt IfStmt {..} = do
  cond' <- ssaExpr ifStmtCondition
  (then', else') <- ssaBranches ifStmtThen ifStmtElse
  IfStmt cond' then' else' <$> freshId StmtId
ssaStmt Skip {..}    = Skip <$> freshId StmtId


-- -----------------------------------------------------------------------------
ssaExpr :: FD r => Expr a -> Sem r (Expr Int)
-- -----------------------------------------------------------------------------
ssaExpr Constant {..} = Constant constantValue <$> freshId NoId
ssaExpr Variable {..} = ssaVariable Variable { .. }
ssaExpr UF {..}       = do
  args <- traverse ssaExpr ufArgs
  n    <- freshId FunId
  return $ UF (name n) args n
  where name n = "uf_" <> ufName <> "_" <> T.pack (show n)
ssaExpr IfExpr {..} =
  IfExpr
    <$> ssaExpr ifExprCondition
    <*> ssaExpr ifExprThen
    <*> ssaExpr ifExprElse
    <*> freshId NoId
ssaExpr Str {..} = Str strValue <$> freshId NoId
ssaExpr Select {..} =
  Select
    <$> ssaExpr selectVar
    <*> traverse ssaExpr selectIndices
    <*> freshId NoId

ssaModuleInstance :: FD r => ModuleInstance a -> Sem r (ModuleInstance Int)
ssaModuleInstance ModuleInstance{..} = do
  ports' <- traverse ssaExpr moduleInstancePorts
  n <- freshId StmtId
  return $
    ModuleInstance { moduleInstancePorts = ports'
                   , moduleInstanceData  = n
                   , ..
                   }

-- -----------------------------------------------------------------------------
-- Implementation details
-- -----------------------------------------------------------------------------

data IdType = ModId | ABId | StmtId | FunId | NoId

type Vars = HS.HashSet Id
type VarId = HM.HashMap Id Int
-- type VarIds = HM.HashMap Id IS.IntSet

type TRNextVars = IM.IntMap VarId

data St = St { modId           :: Int -- id for modules
             , abId            :: Int -- id for always blocks
             , stmtId          :: Int -- id for statements
             , funId           :: Int -- id for functions

             -- these are local to the statements
             , varMaxIds       :: VarId  -- id for the vars
             , lastBlocking    :: VarId  -- last blocking assignment of the vars
             , lastNonBlocking :: VarId -- non blocking assignments of the vars

             , trNextVars      :: TRNextVars -- stmt id -> var id -> last variable
             }

initialSt :: St
initialSt = St { modId           = 0
               , abId            = 0
               , stmtId          = 0
               , funId           = 0
               , varMaxIds       = mempty
               , lastBlocking    = mempty
               , lastNonBlocking = mempty
               , trNextVars      = mempty
               }

runSSA :: Sem (State St ': r) a -> Sem r (a, TRNextVars)
runSSA act = do
  (st, res) <- act & runState initialSt
  return (res, trNextVars st)

ssaContinousAssignment :: FD r => Expr a -> Expr a -> Sem r (Stmt Int)
ssaContinousAssignment lhs rhs = do
  lhs' <- freshVariable lhs
  rhs' <- ssaExpr rhs
  updateLastBlocking lhs'
  Assignment Continuous lhs' rhs' <$> freshId StmtId

ssaBlockingAssignment :: FD r => Expr a -> Expr a -> Sem r (Stmt Int)
ssaBlockingAssignment lhs rhs = do
  lhs' <- freshVariable lhs
  rhs' <- ssaExpr rhs
  updateLastBlocking lhs'
  Assignment Blocking lhs' rhs' <$> freshId StmtId

ssaNonBlockingAssignment :: FD r => Expr a -> Expr a -> Sem r (Stmt Int)
ssaNonBlockingAssignment lhs rhs = do
  lhs' <- freshVariable lhs
  rhs' <- ssaExpr rhs
  updateLastNonBlocking lhs'
  Assignment NonBlocking lhs' rhs' <$> freshId StmtId

ssaVariable :: FD r => Expr a -> Sem r (Expr Int)
ssaVariable var =
  let name = varName var
  in  Variable name (varModuleName var)
      <$> gets (HM.lookupDefault 0 name . lastBlocking)

freshId :: FD r => IdType -> Sem r Int
freshId = \case
  ModId  -> gets modId <* modify (\St {..} -> St { modId = modId + 1, .. })
  ABId   -> gets abId <* modify (\St {..} -> St { abId = abId + 1, .. })
  StmtId -> gets stmtId <* modify (\St {..} -> St { stmtId = stmtId + 1, .. })
  FunId  -> gets funId <* modify (\St {..} -> St { funId = funId + 1, .. })
  NoId   -> return 0

-- creates phi nodes only for the blocking assignments
ssaBranches :: FDM2 r => Stmt a -> Stmt a -> Sem r (Stmt Int, Stmt Int)
ssaBranches br1 br2 = do
  st_init <- getState

  br1' <- ssaStmt br1
  (m1,lb1,lnb1) <- getState

  setState st_init

  br2' <- ssaStmt br2
  (m2,lb2,lnb2) <- getState

  (lb', b1, b2)    <- balanceBranches Blocking (lb1, lb2)
  (lnb', nb1, nb2) <- balanceBranches NonBlocking (lnb1, lnb2)

  let m' = HM.unionWith max m1 m2
  setState (m', lb', lnb')

  (,) <$>
    ext br1' (b1 <> nb1) <*>
    ext br2' (b2 <> nb2)

  where
    getState =
      (,,) <$> gets varMaxIds <*> gets lastBlocking <*> gets lastNonBlocking
    setState (m, lb, lnb) =
      modify $ \st -> st {varMaxIds = m, lastBlocking = lb, lastNonBlocking = lnb}
    ext s1 s2 =
      case s2 of
        SQ.Empty -> return s1
        _        -> Block (s1 <| s2) <$> freshId StmtId

balanceBranches :: FDM r => AssignmentType -> (VarId, VarId) -> Sem r (VarId, L (Stmt Int), L (Stmt Int))
balanceBranches t (m1, m2) = do
  modName <- asks getModuleName
  let mkStmt (v,oldN,newN) =
        Assignment t
        (Variable v modName newN)
        (Variable v modName oldN) <$>
        freshId StmtId
  s1 <- traverse mkStmt diff1
  s2 <- traverse mkStmt diff2
  return (m, s1, s2)
  where
    m = HM.unionWith max m1 m2
    diffF mInit acc v n =
      case HM.lookup v mInit of
        Nothing -> acc |> (v, 0, n)
        Just ni -> if | n > ni    -> acc |> (v, ni, n)
                      | n == ni   -> acc
                      | otherwise -> error "expected (n >= ni)"
    diff1 = HM.foldlWithKey' (diffF m1) SQ.empty m
    diff2 = HM.foldlWithKey' (diffF m2) SQ.empty m

withAB :: AlwaysBlock a -> Sem r b -> Sem r b
withAB _ = id

-- type WithStmtSt = (VarId, L (Stmt Int))

withStmt :: FD r => Sem r (Stmt Int) -> Sem r (Stmt Int)
withStmt act = do
  modify $ \st -> st { varMaxIds       = mempty
                     , lastBlocking    = mempty
                     , lastNonBlocking = mempty }
  stmt' <- act
  modify $ \st -> st { trNextVars = IM.insert (stmtData stmt')
                                    (HM.union (lastBlocking st) (lastNonBlocking st))
                                    (trNextVars st)
                     }
  return stmt'


freshVariable :: FD r => Expr a -> Sem r (Expr Int)
freshVariable var = do
  let name = varName var
  modify $ \St {..} ->
    St { varMaxIds = HM.alter (Just . maybe 1 (+ 1)) name varMaxIds, .. }
  Variable name (varModuleName var) <$> gets ((HM.! name) . varMaxIds)

updateLastBlocking :: FD r => Expr Int -> Sem r ()
updateLastBlocking var = modify
  $ \st@St {..} -> st { lastBlocking = HM.insert name varId lastBlocking }
 where
  name  = varName var
  varId = exprData var

updateLastNonBlocking :: FD r => Expr Int -> Sem r ()
updateLastNonBlocking var = modify
  $ \st@St {..} -> st { lastNonBlocking = HM.insert name varId lastNonBlocking }
 where
  name  = varName var
  varId = exprData var
