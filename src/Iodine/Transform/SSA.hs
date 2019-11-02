{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Transform.SSA
  ( ssa
  , SSAIR
  )
where

import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.IR
import Iodine.Language.Types

import           Data.Function
import qualified Data.HashMap.Strict      as HM
import qualified Data.Sequence            as SQ

import Polysemy
import Polysemy.State

data St = St { abId          :: Int               -- id for always blocks
             , stmtId        :: Int               -- id for statements
             , varMaxId      :: HM.HashMap Id Int -- max id of the variable
             , varIds        :: HM.HashMap Id Int -- current id of the variable
             , currentModule :: Id                -- name of the current module
             }

type SSAIR = L (Module Int)

type FD r = Member (State St) r

-- -----------------------------------------------------------------------------
-- after this step, each new variable, statement, and always block
-- within a module will have an unique id
ssa :: ParsedIR -> SSAIR
-- -----------------------------------------------------------------------------
ssa = fmap $ \m@Module{..} ->
  ssaModule m
  & evalState (initialSt moduleName)
  & run
  where
    initialSt name = St 0 0 HM.empty HM.empty name

-- -----------------------------------------------------------------------------
ssaModule :: FD r => Module a -> Sem r (Module Int)
-- -----------------------------------------------------------------------------
ssaModule Module{..} =
  Module moduleName ports variables <$>
  traverse ssaStmtSingle gateStmts <*>
  traverse ssaAB alwaysBlocks <*>
  return 0

-- -----------------------------------------------------------------------------
ssaAB :: FD r => AlwaysBlock a -> Sem r (AlwaysBlock Int)
-- -----------------------------------------------------------------------------
ssaAB AlwaysBlock{..} =
  AlwaysBlock (const 0 <$> abEvent) <$>
  ssaStmtSingle abStmt <*>
  (modify (incrAbId 1) *> gets abId)
  where 
    incrAbId n St{..} = St { abId = abId + n, .. }

-- -----------------------------------------------------------------------------
ssaStmt :: FD r => Stmt a -> Sem r (Stmt Int)
-- -----------------------------------------------------------------------------
ssaStmt Block{..} = Block <$> traverse ssaStmt blockStmts <*> freshStmtId

ssaStmt ModuleInstance{..} = ModuleInstance moduleInstanceType moduleInstanceName <$>
                             traverse ssaExpr moduleInstancePorts <*>
                             freshStmtId

ssaStmt Assignment{..} = do
  rhs' <- ssaExpr assignmentRhs
  let lhsVarName = varName assignmentLhs
  lhs' <- freshVariable lhsVarName
  updateVariableId lhsVarName (exprData lhs')
  Assignment assignmentType lhs' rhs' <$> freshStmtId

ssaStmt IfStmt{..} = do
  initVarIdMap <- gets varIds

  cond' <- ssaExpr ifStmtCondition

  then' <- ssaStmt ifStmtThen
  thenVarIdMap <- gets varIds
  modify (\s -> s { varIds = initVarIdMap })

  else' <- ssaStmt ifStmtElse
  elseVarIdMap <- gets varIds

  ifStmt' <- IfStmt cond' then' else' <$> freshStmtId
  phiNodes <- makePhiNodes thenVarIdMap elseVarIdMap

  case phiNodes of
    SQ.Empty -> return ifStmt'
    _ -> Block (ifStmt' SQ.<| phiNodes) <$> freshStmtId

ssaStmt PhiNode{..} = error "ssaStmt should not be called on a PhiNode!"

ssaStmt Skip{..} = Skip <$> freshStmtId

type M = HM.HashMap Id Int

makePhiNodes :: FD r => M -> M -> Sem r (L (Stmt Int))
makePhiNodes m1 m2 = sequence $ HM.foldlWithKey' go SQ.empty m1
  where
    go actions varName varId1 =
      let varId2 = HM.lookupDefault 0 varName m2
      in if   varId1 == varId2
         then actions
         else actions SQ.|>
              do lhs <- freshVariable varName
                 varModuleName <- gets currentModule
                 let rhs = Variable { exprData = varId1, .. } SQ.<|
                           Variable { exprData = varId2, .. } SQ.<|
                           SQ.empty
                 updateVariableId varName (exprData lhs)
                 PhiNode lhs rhs <$> freshStmtId


ssaStmtSingle :: FD r => Stmt a -> Sem r (Stmt Int)
ssaStmtSingle stmt = ssaStmt stmt <* resetStmtState
  where
    resetStmtState = do
      modify $ \s -> s { varMaxId = HM.empty }
      modify $ \s -> s { varIds   = HM.empty }

-- -----------------------------------------------------------------------------
ssaExpr :: FD r => Expr a -> Sem r (Expr Int)
-- -----------------------------------------------------------------------------
ssaExpr e@Constant{..} = return $ const 0 <$> e
ssaExpr Variable{..} = getVariableId varName
ssaExpr UF{..} = UF ufName <$>
                 traverse ssaExpr ufArgs <*>
                 noId
ssaExpr IfExpr{..} = IfExpr <$>
                     ssaExpr ifExprCondition <*>
                     ssaExpr ifExprThen <*>
                     ssaExpr ifExprElse <*>
                     noId
ssaExpr e@Str{..} = return $ const 0 <$> e
ssaExpr Select{..} = Select <$>
                     ssaExpr selectVar <*>
                     traverse ssaExpr selectIndices <*>
                     noId


-- helper functions

freshStmtId :: FD r => Sem r Int
freshStmtId = do
  modify $ \St{..} -> St {stmtId = stmtId + 1, .. }
  gets stmtId

getVariableId :: FD r => Id -> Sem r (Expr Int)
getVariableId varName = do
  n <- maybe 0 id . HM.lookup varName <$> gets varIds
  varModuleName <- gets currentModule
  return Variable{ exprData = n, .. }

updateVariableId :: FD r => Id -> Int -> Sem r ()
updateVariableId varName varId =
  modify $ \St{..} -> St { varIds = HM.alter (\case _ -> Just varId) varName varIds
                         , .. }

freshVariable :: FD r => Id -> Sem r (Expr Int)
freshVariable varName = do
  freshId <- maybe 1 (+1) . HM.lookup varName <$> gets varMaxId
  varModuleName <- gets currentModule
  return Variable{ exprData = freshId, .. }

noId :: Sem r Int
noId = return 0
  
