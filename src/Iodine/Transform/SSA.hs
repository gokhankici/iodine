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
import           Iodine.Language.Types

import           Control.Lens
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntSet                   as IS
import qualified Data.IntMap                   as IM
import           Data.Maybe
import qualified Data.Sequence                 as SQ

import           Polysemy
import           Polysemy.State

type FD r = Member (State St) r
type SSAIR = L (Module Int)
type SSAOutput = (SSAIR, TRNextVars)

-- -----------------------------------------------------------------------------
-- after this step, each new variable, statement, and always block
-- within a module will have an unique id
ssa :: ParsedIR -> Sem r SSAOutput
-- -----------------------------------------------------------------------------
ssa modules = traverse ssaModule modules & runSSA

-- -----------------------------------------------------------------------------
ssaModule :: FD r => Module a -> Sem r (Module Int)
-- -----------------------------------------------------------------------------
ssaModule m@Module {..} =
  withModule m
    $   Module moduleName ports variables
    <$> traverse ssaStmtSingle gateStmts
    <*> traverse ssaAB         alwaysBlocks
    <*> freshId ModId

-- -----------------------------------------------------------------------------
ssaAB :: FD r => AlwaysBlock a -> Sem r (AlwaysBlock Int)
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
ssaStmtSingle :: FD r => Stmt a -> Sem r (Stmt Int)
-- -----------------------------------------------------------------------------
ssaStmtSingle s = withStmt $ ssaStmt s

ssaStmt :: FD r => Stmt a -> Sem r (Stmt Int)
ssaStmt Block {..} = Block <$> traverse ssaStmt blockStmts <*> freshId StmtId
ssaStmt Assignment {..} = case assignmentType of
  Continuous  -> ssaContinousAssignment assignmentLhs assignmentRhs
  Blocking    -> ssaBlockingAssignment assignmentLhs assignmentRhs
  NonBlocking -> ssaNonBlockingAssignment assignmentLhs assignmentRhs
ssaStmt IfStmt {..} = do
  cond'                     <- ssaExpr ifStmtCondition
  (then', else', phiNodes') <- ssaBranches ifStmtThen ifStmtElse
  ifStmt'                   <- IfStmt cond' then' else' <$> freshId StmtId
  case phiNodes' of
    SQ.Empty -> return ifStmt'
    _        -> Block (ifStmt' SQ.<| phiNodes') <$> freshId StmtId
ssaStmt ModuleInstance {..} = do
  ports' <- traverse ssaExpr moduleInstancePorts
  n      <- freshId StmtId
  return ModuleInstance { moduleInstancePorts = ports', stmtData = n, .. }
ssaStmt PhiNode {..} = error "ssa should not run on phi nodes"
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



-- -----------------------------------------------------------------------------
-- Implementation details
-- -----------------------------------------------------------------------------

data IdType = ModId | ABId | StmtId | FunId | NoId

type VarId = HM.HashMap Id Int
type VarIds = HM.HashMap Id IS.IntSet

type TRNextVars = IM.IntMap VarId

data St = St { modId         :: Int -- id for modules
             , abId          :: Int -- id for always blocks
             , stmtId        :: Int -- id for statements
             , funId         :: Int -- id for functions

             , currentModule :: Maybe Id -- name of the current module

             -- these are local to the statements
             , varMaxIds     :: VarId  -- id for the vars
             , lastBlocking  :: VarId  -- last blocking assignment of the vars
             , nonBlockings  :: VarIds -- non blocking assignments of the vars

             , trNextVars    :: TRNextVars -- stmt id -> var id -> last variable
             }

initialSt :: St
initialSt = St { modId         = 0
               , abId          = 0
               , stmtId        = 0
               , funId         = 0
               , currentModule = mempty
               , varMaxIds     = mempty
               , lastBlocking  = mempty
               , nonBlockings  = mempty
               , trNextVars    = mempty
               }

runSSA :: Sem (State St ': r) a -> Sem r (a, TRNextVars)
runSSA act = do
  (st, res) <- runState initialSt act
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
  updateNonBlocking lhs'
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

ssaBranches
  :: FD r => Stmt a -> Stmt a -> Sem r (Stmt Int, Stmt Int, L (Stmt Int))
ssaBranches br1 br2 = do
  lastBlocking_init <- gets lastBlocking

  br1'              <- ssaStmt br1
  lastBlocking1     <- gets lastBlocking

  modify (\s -> s { lastBlocking = lastBlocking_init })

  br2'          <- ssaStmt br2
  lastBlocking2 <- gets lastBlocking

  phiNodes      <- makePhiNodes lastBlocking1 lastBlocking2

  return (br1', br2', phiNodes)

makePhiNodes :: FD r => VarId -> VarId -> Sem r (L (Stmt Int))
makePhiNodes m1 m2 = sequence $ HM.foldlWithKey' go SQ.empty m1
 where
  go actions varName varId1 =
    let varId2 = HM.lookupDefault 0 varName m2
    in  if varId1 == varId2
          then actions
          else actions SQ.|> do
            varModuleName <- gets (fromJust . currentModule)
            lhs           <- freshVariable $ Variable { exprData = (), .. }
            let rhs =
                  Variable { exprData = varId1, .. }
                    SQ.<| Variable { exprData = varId2, .. }
                    SQ.<| SQ.empty
            updateLastBlocking lhs
            phiNode lhs rhs <$> freshId StmtId

withModule :: FD r => Module a -> Sem r b -> Sem r b
withModule Module {..} act =
  modify (\St {..} -> St { currentModule = Just moduleName, .. })
    *> act
    <* modify (\St {..} -> St { currentModule = Nothing, .. })

withAB :: AlwaysBlock a -> Sem r b -> Sem r b
withAB _ = id

type WithStmtSt = (VarId, L (Stmt Int))

withStmt :: FD r => Sem r (Stmt Int) -> Sem r (Stmt Int)
withStmt act = do
  oldSt <- get
  modify $ \st ->
    st { varMaxIds = mempty, lastBlocking = mempty, nonBlockings = mempty }
  stmt'                            <- act
  nbs                              <- gets nonBlockings

  ((lastNonBlocking, phiNodes), _) <-
    HM.traverseWithKey
        (\varName varIds -> do
          varModuleName <- gets (fromJust . currentModule)
          let phiRhs = IS.foldl'
                (\s varId -> s SQ.|> Variable { exprData = varId, .. })
                SQ.empty
                varIds
          case phiRhs of
            SQ.Empty -> undefined
            r SQ.:<| SQ.Empty ->
              modify @WithStmtSt (_1 . at varName ?~ exprData r)
            _ -> do
              phiLhs <- freshVariable Variable { exprData = (), .. }
              phi    <- phiNode phiLhs phiRhs <$> freshId StmtId
              modify @WithStmtSt (_1 . at varName ?~ exprData phiLhs)
              modify @WithStmtSt (_2 %~ (|> phi))
        )
        nbs
      & runState (mempty, mempty)

  stmt'' <- if SQ.null phiNodes
    then return stmt'
    else Block (stmt' SQ.<| phiNodes) <$> freshId StmtId

  modify $ \st -> st
    { varMaxIds    = varMaxIds oldSt
    , lastBlocking = lastBlocking oldSt
    , nonBlockings = nonBlockings oldSt
    , trNextVars   = IM.insert (stmtData stmt'')
                               (HM.union (lastBlocking st) lastNonBlocking)
                               (trNextVars st)
    }
  return stmt''

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

updateNonBlocking :: FD r => Expr Int -> Sem r ()
updateNonBlocking var = modify
  $ \st@St {..} -> st { nonBlockings = HM.alter helper name nonBlockings }
 where
  name   = varName var
  varId  = exprData var
  helper = Just . maybe (IS.singleton varId) (IS.insert varId)

phiNode :: Expr a -> L (Expr a) -> a -> Stmt a
phiNode lhs rhs a = if SQ.length rhs < 2
  then error "phinode rhs count must be > 1"
  else PhiNode lhs rhs a
