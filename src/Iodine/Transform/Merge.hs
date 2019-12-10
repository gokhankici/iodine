{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Iodine.Transform.Merge (merge) where

import           Iodine.Analyze.ModuleSummary
import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Language.IRParser
import           Iodine.Types
import           Iodine.Utils

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query as GQ
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.List (elem, intercalate)
import qualified Data.Sequence as SQ
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.State

type DepGraph = Gr () ()
type StmtMap  = IM.IntMap (Stmt ())
data St =
  St { _writtenBy   :: HM.HashMap Id IS.IntSet
     , _readBy      :: HM.HashMap Id IS.IntSet
     , _stmtCounter :: Int
     , _stmtMap     :: StmtMap
     }

makeLenses ''St

type G r = Members '[Reader AnnotationFile , Reader SummaryMap , Reader ModuleMap , Error IodineException] r

-- -----------------------------------------------------------------------------
merge :: G r => ParsedIR -> Sem r ParsedIR
-- -----------------------------------------------------------------------------
merge = traverse mergeModule

-- | make gate statements a always* block, and merge with the rest
mergeModule :: G r => Module () -> Sem r (Module ())
mergeModule Module {..} = do
  (miBlocks, moduleInstances') <- tryToSummarize moduleInstances
  alwaysBlocks' <- mergeAlwaysBlocks $ alwaysBlocks <> gateBlocks <> miBlocks
  return $
    Module { alwaysBlocks    = alwaysBlocks'
           , gateStmts       = mempty
           , moduleInstances = moduleInstances'
           , ..
           }
  where
    gateBlocks = makeStarBlock <$> gateStmts

{- |
All blocks with the same non-star event are merged into a single block (with
random ordering). For the always* blocks, a dependency graph is created first
and then they are merged according to their order in the directed-acyclic graph.
-}
mergeAlwaysBlocks :: G r => L (AlwaysBlock ()) -> Sem r (L (AlwaysBlock ()))
mergeAlwaysBlocks as =
  foldlM' mempty (HM.toList eventMap) $ \acc (e, stmts) -> do
    ab' <-
      case e of
        Star -> mergeAlwaysStarBlocks stmts
        _    -> return $ mergeAlwaysEventBlocks e stmts
    return $ acc |> ab'
  where
    eventMap = foldl' updateM mempty as
    updateM m AlwaysBlock{..} = HM.alter (append (SQ.<|) abStmt) abEvent m


{- |
Merge every always* block into a single one. The statements that belong to the
same connected components are ordered according to their topological sort.
However, the order between connected components are random.
-}
mergeAlwaysStarBlocks :: G r => L (Stmt ()) -> Sem r (AlwaysBlock ())
mergeAlwaysStarBlocks stmts = do
  (depGraph, stmtIds) <- buildDependencyGraph stmts
  unless (G.noNodes depGraph == SQ.length stmts) $
    throw . IE Merge  $ "graph size does not match up with the initial statements"
  let components = G.components depGraph
      graphs = (\ns -> G.nfilter (`elem` ns) depGraph) <$> components
  stmts' <- foldlM' mempty graphs $ \acc g -> do
    let stmtOrder = GQ.topsort g
    when (hasCycle g) $
      throw . IE Merge $
        "star dependency graph has a loop:\n" ++
        intercalate "\n" (show . (stmtIds IM.!) <$> stmtOrder)
    return $ (stmtIds IM.!) <$> stmtOrder
             & SQ.fromList
             & (acc <>)
  return $ makeStarBlock (Block stmts' ())

{- |
merge the always blocks with the same non-star event after makign sure that
their dependecy graph form a DAG
-}
mergeAlwaysEventBlocks :: Event () -> L (Stmt ()) -> AlwaysBlock ()
mergeAlwaysEventBlocks e stmts = AlwaysBlock e stmt'
  where
    stmt' =
      case stmts of
        SQ.Empty          -> error "this should be unreachable"
        s SQ.:<| SQ.Empty -> s
        _                 -> Block stmts ()

type ModuleMap = HM.HashMap Id (Module ())
type FD r = Members '[State St, Reader ModuleMap] r

{- |
builds a dependency graph where (s1, s2) \in G iff there exists a variable v
such that s1 updates v and s2 reads v
-}
buildDependencyGraph :: Member (Reader ModuleMap) r
                     => L (Stmt ()) -> Sem r (DepGraph, StmtMap)
buildDependencyGraph stmts =
  traverse_ update stmts
  & runState initialState
  & fmap (\(st, _) -> ( buildGraph (st ^. readBy) (st ^. writtenBy) (st ^. stmtMap & IM.keysSet)
                      , st ^. stmtMap
                      ))
  where
    buildGraph readMap writeMap nodes =
      HM.foldlWithKey'
      (\g v fromIds ->
         case HM.lookup v readMap of
           Nothing    -> g
           Just toIds ->
             IS.foldl'
             (\g2 fromId ->
                IS.foldr'
                (\toId -> G.insEdge (fromId, toId, ()))
                g2
                toIds
             )
             g
             fromIds
      )
      (IS.foldr' (\n -> G.insNode (n, ())) G.empty nodes)
      writeMap

    -- create a new id for the given statement, and update its read & write set
    update :: FD r => Stmt () -> Sem r ()
    update s = do
      n <- gets (^. stmtCounter)
      modify $
        (stmtCounter +~ 1) .
        (stmtMap . at n ?~ s)
      updateN n s

    -- update read & write sets for the given statement id
    updateN :: FD r => Int -> Stmt () -> Sem r ()
    updateN n Block {..} = traverse_ (updateN n) blockStmts
    updateN n Assignment {..} = do
      forM_ (getVariables assignmentLhs) (insertToWrites n)
      forM_ (getVariables assignmentRhs) (insertToReads n)
    updateN n IfStmt   {..} = do
      forM_ (getVariables ifStmtCondition) (insertToReads n)
      updateN n ifStmtThen
      updateN n ifStmtElse
    updateN _ Skip {..} = return ()
    updateN n SummaryStmt {..} = do
      Module{..} <- asks (HM.! summaryType)
      for_ ports $ \case
        Input i ->
          for_ (getVariables (summaryPorts HM.! variableName i)) (insertToReads n)
        Output o ->
          for_ (getVariables (summaryPorts HM.! variableName o)) (insertToWrites n)



    insertToWrites n v =
      modify $ writtenBy %~ HM.alter (append IS.insert n) v

    insertToReads n v =
      modify $ readBy %~ HM.alter (append IS.insert n) v

initialState :: St
initialState = St mempty mempty 0 mempty

makeStarBlock :: Stmt () -> AlwaysBlock ()
makeStarBlock = AlwaysBlock Star

{- |
given a updater function and an element, create a helper function to be used
with `HM.alter`
-}
append :: Monoid m => (a -> m -> m) -> a -> Maybe m -> Maybe m
append f a = \case
  Nothing -> Just $ f a mempty
  Just m  -> Just $ f a m

{- |
if a graph does not have a cycle, each strongly connected component of the graph
should consist of a single element
-}
hasCycle :: DepGraph -> Bool
hasCycle g = length (GQ.scc g) /= G.noNodes g
