{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Analyze.DependencyGraph
  (
    dependencyGraph
  , dependencyGraphFromMany
  , VarDepGraph
  , VarDepEdgeType(..)
  )
where

import           Iodine.Language.IR
import           Iodine.Types

import           Control.Lens
import           Data.Foldable
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import           Polysemy
import           Polysemy.State

type VarDepGraph = Gr () VarDepEdgeType
data VarDepEdgeType = Implicit | Explicit AssignmentType

data St =
  St { _depGraph   :: VarDepGraph
     , _pathVars   :: IS.IntSet
     , _varMap     :: HM.HashMap Id Int
     , _varCounter :: Int
     }

makeLenses ''St

{- |
Creates a dependency graph for the variables that occur inside the given
statement.

- (v1, v2, Explicit) \in E iff v1's value directly affects the value of v2

- (v1, v2, Implicit) \in E iff v1 is a path variable (occurs inside the
  condition of an if statement where v2 is assigned)
-}
dependencyGraph :: Stmt a -> (VarDepGraph, HM.HashMap Id Int)
dependencyGraph stmt = dependencyGraphFromMany [stmt]

{- |
Like 'dependencyGraph', but creates a graph from many statements.
-}
dependencyGraphFromMany :: Foldable t
                        => t (Stmt a)
                        -> (VarDepGraph, HM.HashMap Id Int)
dependencyGraphFromMany stmts = (st ^. depGraph, st ^. varMap)
  where
    st = traverse_ handleStmt stmts
         & runState initialState
         & run
         & fst
    initialState =
      St { _depGraph   = G.empty
         , _pathVars   = mempty
         , _varMap     = mempty
         , _varCounter = 0
         }


type FD r = Member (State St) r

handleStmt :: FD r => Stmt a -> Sem r ()
handleStmt Skip{..}  = return ()
handleStmt Block{..} = traverse_ handleStmt blockStmts
handleStmt Assignment{..} =
  case assignmentLhs of
    Variable{..} -> do
      lhsNode <- getNode varName
      rhsNodes <- getNodes (getVariables assignmentRhs)
      for_ (IS.toList rhsNodes) $ \rhsNode ->
        addNode (rhsNode, lhsNode, Explicit assignmentType)
      pathNodes <- gets (^. pathVars)
      for_ (IS.toList pathNodes) $ \pathNode ->
        addNode (pathNode, lhsNode, Implicit)
    _ -> error "assignment lhs is non-variable"
handleStmt IfStmt{..} = do
  currentSt <- get
  newPathVars <- getNodes (getVariables ifStmtCondition)
  modify $ pathVars %~ IS.union newPathVars
  traverse_ handleStmt [ifStmtThen, ifStmtElse]
  modify $ pathVars .~ (currentSt ^. pathVars)
handleStmt SummaryStmt{..} =
  error "unreachable" -- there shouldn't be any summary stmt at this point

addNode :: FD r => (Int, Int, VarDepEdgeType) -> Sem r ()
addNode edge@(fromNode, toNode, _) = do
  for_ [fromNode, toNode] $ \n ->
    modify $ depGraph %~ G.insNode (n, ())
  modify $ depGraph %~ G.insEdge edge


getNodes :: FD r => HS.HashSet Id -> Sem r IS.IntSet
getNodes = fmap IS.fromList . traverse getNode . HS.toList


getNode :: FD r => Id -> Sem r Int
getNode v = do
  res <- gets (^. varMap . to (HM.lookup v))
  case res of
    Nothing -> do
      n <- gets (^. varCounter)
      modify $
        (varCounter +~ 1) .
        (varMap %~ HM.insert v n)
      return n
    Just n -> return n
