{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Iodine.Transform.Merge (merge) where

import Iodine.Types
import Iodine.Language.IR
import Iodine.Language.IRParser

import           Control.Lens
import           Data.Foldable
import           Data.List             (elem, intercalate)
import qualified Data.IntSet           as IS
import qualified Data.IntMap           as IM
import qualified Data.HashMap.Strict   as HM
import qualified Data.Sequence         as SQ
import qualified Data.Graph.Inductive  as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query as GQ
import           Polysemy
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

type FD r = Member (State St) r

-- -----------------------------------------------------------------------------
merge :: ParsedIR -> ParsedIR
-- -----------------------------------------------------------------------------
merge = fmap mergeModule


-- | make gate statements a always* block, and merge with the rest
mergeModule :: Module () -> Module ()
mergeModule Module {..} =
  Module { alwaysBlocks = mergeAlwaysBlocks $ alwaysBlocks <> gateBlocks
         , gateStmts    = mempty
         , ..
         }
  where
    gateBlocks = makeStarBlock <$> gateStmts


{- |
All blocks with the same non-star event are merged into a single block (with
random ordering). For the always* blocks, a dependency graph is created first
and then they are merged according to their order in the directed-acyclic graph.
-}
mergeAlwaysBlocks :: L (AlwaysBlock ()) -> L (AlwaysBlock ())
mergeAlwaysBlocks as = HM.foldlWithKey' (\acc e ss-> acc SQ.>< mkBlocks e ss) mempty eventMap
  where
    mkBlocks e stmts =
      SQ.singleton $
      case e of
        Star        -> mergeAlwaysStarBlocks stmts
        PosEdge{..} -> mergeAlwaysEventBlocks e stmts
        NegEdge{..} -> mergeAlwaysEventBlocks e stmts
    eventMap = foldl' updateM mempty as
    updateM m AlwaysBlock{..} = HM.alter (append (SQ.<|) abStmt) abEvent m


{- |
Merge every always* block into a single one. The statements that belong to the
same connected components are ordered according to their topological sort.
However, the order between connected components are random.
-}
mergeAlwaysStarBlocks :: L (Stmt ()) -> AlwaysBlock ()
mergeAlwaysStarBlocks stmts =
  if   G.noNodes depGraph == SQ.length stmts
  then makeStarBlock (Block stmts' ())
  else error $ "graph size does not match up with the initial statements"
  where
    (depGraph, stmtIds) = buildDependencyGraph stmts
    components = G.components depGraph
    graphs = (\ns -> G.nfilter (`elem` ns) depGraph) <$> components
    stmts' =
      foldl'
      (\acc g ->
         let _stmtOrder = GQ.topsort g
             stmtOrder =
               if hasCycle g
               then error $
                    "star dependency graph has a loop:\n" ++
                    intercalate "\n" ((show . (stmtIds IM.!)) <$> _stmtOrder)
               else _stmtOrder
         in (stmtIds IM.!) <$> stmtOrder
            & SQ.fromList
            & (acc <>)
      )
      mempty
      graphs

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


{- |
builds a dependency graph where (s1, s2) \in G iff there exists a variable v
such that s1 updates v and s2 reads v
-}
buildDependencyGraph :: L (Stmt ()) -> (DepGraph, StmtMap)
buildDependencyGraph stmts =
  traverse_ update stmts
  & runState initialState
  & run
  & \(st, _) -> ( buildGraph (st ^. readBy) (st ^. writtenBy) (st ^. stmtMap & IM.keysSet)
                , st ^. stmtMap
                )

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

    insertToWrites n v =
      modify $ writtenBy %~ HM.alter (append IS.insert n) v

    insertToReads n v =
      modify $ readBy %~ HM.alter (append IS.insert n) v

initialState :: St
initialState = St mempty mempty 0 mempty

makeStarBlock :: Stmt () -> AlwaysBlock ()
makeStarBlock s = AlwaysBlock Star s

{- |
given a updater function and an element, create a helper function to be used
with `alter`
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
