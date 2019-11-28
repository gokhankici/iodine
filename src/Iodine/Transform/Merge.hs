{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Iodine.Transform.Merge (merge) where

import Iodine.Language.Types
import Iodine.Language.IR
import Iodine.Language.IRParser
import Iodine.Utils

import           Control.Lens
-- import           Control.Monad
import           Data.Foldable
import           Data.List             (elem)
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


mergeModule :: Module () -> Module ()
mergeModule Module {..} =
  -- make gate statements a always* block, and merge with the rest
  Module { alwaysBlocks = mergeAlwaysBlocks $ alwaysBlocks SQ.>< gateBlocks
         , gateStmts    = mempty
         , ..
         }
  where
    gateBlocks = makeStarBlock <$> gateStmts


-- All blocks with the same non-star event are merged into a single block (with
-- random ordering). For the always* blocks, a dependency graph is created first
-- and then they are merged according to their order in the directed-acyclic
-- graph.
mergeAlwaysBlocks :: L (AlwaysBlock ()) -> L (AlwaysBlock ())
mergeAlwaysBlocks as = HM.foldlWithKey' (\acc e ss-> acc SQ.>< mkBlocks e ss) mempty eventMap
  where
    mkBlocks e stmts =
      case e of
        Star{..}    -> mergeAlwaysStarBlocks stmts
        PosEdge{..} -> SQ.singleton $ mergeAlwaysEventBlocks e stmts
        NegEdge{..} -> SQ.singleton $ mergeAlwaysEventBlocks e stmts
    eventMap = foldl' updateM mempty as
    updateM m AlwaysBlock{..} = HM.alter (append (SQ.<|) abStmt) abEvent m


mergeAlwaysStarBlocks :: L (Stmt ()) -> L (AlwaysBlock ())
mergeAlwaysStarBlocks stmts = makeStarBlock <$> stmtsList
  where
    (depGraph, stmtIds) = buildDependencyGraph stmts
    components = G.components depGraph
    graphs = (\ns -> G.nfilter (`elem` ns) depGraph) <$> components
    stmtsList =
      foldl'
      (\acc g ->
         let stmtOrder =
               if G.hasLoop g
               then error "dependency graph has a loop"
               else GQ.topsort g
         in (stmtIds IM.!) <$> stmtOrder
            & SQ.fromList
            & \case
                  SQ.Empty          -> error "unreachable"
                  s SQ.:<| SQ.Empty -> s
                  ss                -> Block ss ()
            & (acc SQ.|>)
      )
      mempty
      graphs

-- merge the always blocks with the same non-star event after makign sure that
-- their dependecy graph form a DAG
mergeAlwaysEventBlocks :: Event () -> L (Stmt ()) -> AlwaysBlock ()
mergeAlwaysEventBlocks e stmts =
  if   G.hasLoop . fst $ buildDependencyGraph stmts
  then error "dependency graph has a loop"
  else AlwaysBlock e stmt' ()
  where
    stmt' =
      case stmts of
        SQ.Empty          -> error "this should be unreachable"
        s SQ.:<| SQ.Empty -> s
        _                 -> Block stmts ()


-- builds a dependency graph where (s1, s2) \in G iff there exists a variable v
-- such that s1 updates v and s2 reads v
buildDependencyGraph :: L (Stmt ()) -> (DepGraph, StmtMap)
buildDependencyGraph stmts =
  traverse_ update stmts
  & runState initialState
  & run
  & \(st, _) -> ( buildGraph (st ^. readBy) (st ^. writtenBy)
                , st ^. stmtMap
                )

  where
    buildGraph readMap writeMap =
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
      G.empty
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
    updateN _ ModuleInstance {..} = not_supported
    updateN _ Skip {..} = return ()

    insertToWrites n v =
      modify $ writtenBy %~ HM.alter (append IS.insert n) v

    insertToReads n v =
      modify $ readBy %~ HM.alter (append IS.insert n) v

initialState :: St
initialState = St mempty mempty 0 mempty

makeStarBlock :: Stmt () -> AlwaysBlock ()
makeStarBlock s = AlwaysBlock (Star ()) s ()

-- given a updater function and an element, create a helper function to be used
-- with `alter`
append :: Monoid m => (a -> m -> m) -> a -> Maybe m -> Maybe m
append f a = \case
  Nothing -> Just $ f a mempty
  Just m  -> Just $ f a m