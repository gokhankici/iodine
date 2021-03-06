{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Abduction.Graph ( updateAnnotations
                               , toAbductionGraph
                               , G, E
                               ) where

import Iodine.Types
import Iodine.Language.Types
import Iodine.Abduction.Types

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as HS
import qualified Data.IntSet              as IS
import           Data.Foldable
import qualified Data.Graph.Inductive     as Gr
import qualified Data.Sequence            as SQ
import           Text.Printf

-- the following are for testing !!!
import GHC.IO.Unsafe

-- Graph types
type N   = Gr.Node
-- type Adj = Gr.Adj E
type V   = (Id, Bool) -- (variable name, is reg)
type E   = EdgeData
type G   = Gr.Gr V E

type I = (Id, Int) -- Index

updateAnnotations :: IntermediaryA I -> IntermediaryASt I
updateAnnotations (as, st) = result
  where
    result = findSolution st g
    g = toAbductionGraph as

findSolution :: IntermediaryASt I -> G -> IntermediaryASt I
findSolution (annots, qualifiers) g = unsafePerformIO act `seq` result
  where
    result = if   Gr.size g > 1
             then (annots, qualifiers)
             else error "graph is empty!"

    act = evalStateT go (0 :: Int)

    go = bfsM g
         (HS.foldl' (\l (_, n) -> n SQ.<| l) mempty (annots^.sources)) $
         \n -> do cnt <- get
                  lift (printf "Node #%03d: %d\n" cnt n)
                  modify (+ 1)


toAbductionGraph :: ABS I -> G
toAbductionGraph as = foldl' goAB Gr.empty as

-- -----------------------------------------------------------------------------
-- Graph Construction
-- -----------------------------------------------------------------------------

goAB :: G -> AlwaysBlockA I -> G
goAB g ab = g'
  where
    g' = goStmt (ab^.aSt^.ports) (mempty, g)  (ab^.aStmt)

type GoStmtData = (SQ.Seq I)
type GoStmtAcc  = (GoStmtData, G)

goStmt :: SQ.Seq (VarA I) -> GoStmtAcc -> StmtA I -> G
goStmt prts acc@(implicits, g) s =
  case s of
    Skip                -> g
    Block{..}           -> foldl' (helper implicits) g blockStmts
    BlockingAsgn{..}    -> asgn Blocking lhs rhs
    NonBlockingAsgn{..} -> asgn NonBlocking lhs rhs
    IfStmt{..}          -> foldl' (helper (withConds ifCond)) g [thenStmt, elseStmt]
  where
    helper i accG = goStmt prts (i, accG)
    withConds e   = implicits SQ.>< foldVariables e
    asgn aTyp l r = goAsgn aTyp prts acc l r

goAsgn :: AsgnType -> SQ.Seq (VarA I) -> GoStmtAcc -> I -> VExprA I -> G
goAsgn a prts (implicits, g) (lName, lIndex) r = g''
  where
    g'  = foldl' (addEdge Implicit) g implicits
    g'' = foldl' (addEdge Direct) g' (foldVariables r)

    addEdge e_typ g1 (rName, rIndex) =
      Gr.insEdge (rIndex, lIndex, d) .
      addNode (rIndex, rName) .
      addNode (lIndex, lName) $
      g1
      where
        d = EdgeData { edgeType = e_typ
                     , asgnType = a
                     }

    addNode (ind, name) g1 =
      case Gr.lab g1 ind of
        Nothing -> Gr.insNode (ind, (name, regLookup name prts)) g1
        Just _  -> g1

    regLookup :: Id -> SQ.Seq (VarA I) -> Bool
    regLookup name ps =
      case SQ.viewl ps of 
        p SQ.:< rest ->
          if   fst (Iodine.Language.Types.varName p) == name
          then case p of
                 Register {} -> True
                 Wire     {} -> False
          else regLookup name rest
        SQ.EmptyL ->
          error $ printf "could not find %s in ports" name

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

-- | Starting from the given nodes, do a breadth first search and run the given
-- function for each visited node
bfsM :: Monad m => G -> SQ.Seq N -> (N -> m ()) -> m ()
bfsM g nodes f = evalStateT go (IS.empty, nodes)
  where
    go = do
      hd <- uses _2 SQ.viewl
      case hd of
        SQ.EmptyL    -> return ()
        n SQ.:< rest -> do
          _2 .= rest
          visited <- uses _1 (IS.member n)
          when (not visited) $ do
            _1 %= IS.insert n
            _2 %= (SQ.>< SQ.fromList (Gr.suc g n))
            lift (f n)
          go
