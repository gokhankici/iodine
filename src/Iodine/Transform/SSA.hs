{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Iodine.Transform.SSA
  ( ssa
  , SSAIR
  )
where

import Iodine.Language.IRParser (ParsedIR)
import Iodine.Language.IR
import Iodine.Language.Types

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as HM
import qualified Data.Sequence            as SQ
import           GHC.Generics (Generic)

-- import Debug.Trace

data St = St { _abId          :: Int               -- id for always blocks
             , _stmtId        :: Int               -- id for statements
             , _varMaxId      :: HM.HashMap Id Int -- max id of the variable
             , _varIds        :: HM.HashMap Id Int -- current id of the variable
             , _currentModule :: Id                -- name of the current module
             }
        deriving (Generic)

makeLenses ''St

type S = State St
type SSAIR = L (Module Int)

-- -----------------------------------------------------------------------------
-- after this step, each new variable, statement, and always block
-- within a module will have an unique id
ssa :: ParsedIR -> SSAIR
-- -----------------------------------------------------------------------------
ssa = fmap (\m@Module{..} -> evalState (ssaModule m) (initialSt moduleName))
  where
    initialSt name = St 0 0 HM.empty HM.empty name

-- -----------------------------------------------------------------------------
ssaModule :: Module a -> S (Module Int)
-- -----------------------------------------------------------------------------
ssaModule Module{..} =
  Module moduleName ports variables <$>
  traverse ssaStmtSingle gateStmts <*>
  traverse ssaAB alwaysBlocks <*>
  return 0

-- -----------------------------------------------------------------------------
ssaAB :: AlwaysBlock a -> S (AlwaysBlock Int)
-- -----------------------------------------------------------------------------
ssaAB AlwaysBlock{..} =
  AlwaysBlock (const 0 <$> abEvent) <$>
  ssaStmtSingle abStmt <*>
  ((abId += 1) *> use abId)

-- -----------------------------------------------------------------------------
ssaStmt :: Stmt a -> S (Stmt Int)
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
  initVarIdMap <- use varIds

  cond' <- ssaExpr ifStmtCondition

  then' <- ssaStmt ifStmtThen
  thenVarIdMap <- use varIds
  varIds .= initVarIdMap

  else' <- ssaStmt ifStmtElse
  elseVarIdMap <- use varIds

  ifStmt' <- IfStmt cond' then' else' <$> freshStmtId
  phiNodes <- makePhiNodes thenVarIdMap elseVarIdMap

  case phiNodes of
    SQ.Empty -> return ifStmt'
    _ -> Block (ifStmt' SQ.<| phiNodes) <$> freshStmtId

ssaStmt PhiNode{..} = error "ssaStmt should not be called on a PhiNode!"

ssaStmt Skip{..} = Skip <$> freshStmtId

type M = HM.HashMap Id Int

makePhiNodes :: M -> M -> S (L (Stmt Int))
makePhiNodes m1 m2 = sequence $ HM.foldlWithKey' go SQ.empty m1
  where
    go :: L (S (Stmt Int)) -> Id -> Int -> L (S (Stmt Int))
    go actions varName varId1 =
      let varId2 = HM.lookupDefault 0 varName m2
      in if   varId1 == varId2
         then actions
         else actions SQ.|>
              do lhs <- freshVariable varName
                 varModuleName <- use currentModule
                 let rhs = Variable { exprData = varId1, .. } SQ.<|
                           Variable { exprData = varId2, .. } SQ.<|
                           SQ.empty
                 updateVariableId varName (exprData lhs)
                 PhiNode lhs rhs <$> freshStmtId

ssaStmtSingle :: Stmt a -> S (Stmt Int)
ssaStmtSingle s = ssaStmt s <* resetStmtState

-- -----------------------------------------------------------------------------
ssaExpr :: Expr a -> S (Expr Int)
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

freshStmtId :: S Int
freshStmtId = stmtId += 1 >> use stmtId

getVariableId :: Id -> S (Expr Int)
getVariableId varName = do
  Just n <- varIds . at varName <%= Just . maybe 0 id
  varModuleName <- use currentModule
  return Variable{ exprData = n, .. }

updateVariableId :: Id -> Int -> S ()
updateVariableId varName varId =
  varIds . at varName %= const (Just varId)

freshVariable :: Id -> S (Expr Int)
freshVariable varName = do
  Just n <- varMaxId . at varName <%= Just . maybe 0 (+1)
  varModuleName <- use currentModule
  return Variable{ exprData = n, .. }

noId :: S Int
noId = return 0

resetStmtState :: S ()
resetStmtState = varMaxId .= HM.empty >> varIds .= HM.empty
