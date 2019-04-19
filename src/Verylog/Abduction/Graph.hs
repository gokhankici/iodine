{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Verylog.Abduction.Graph ( updateAnnotations
                               , toAbductionGraph
                               , G, E, EdgeData(..)
                               ) where

import Verylog.Types
import Verylog.Language.Types

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as HS
import qualified Data.IntSet              as IS
import           Data.Foldable
import qualified Data.Graph.Inductive     as Gr
import qualified Data.Sequence            as SQ
import           GHC.Generics hiding (to)
import           Text.Printf
import qualified Data.Aeson as J

-- the following are for testing !!!
import GHC.IO.Unsafe

-- Graph types
data EdgeData = Direct
              | Implicit
              deriving (Eq, Show, Generic)

type N   = Gr.Node
-- type Adj = Gr.Adj E
type V   = Id
type E   = EdgeData
type G   = Gr.Gr V E

type I = (Id, Int) -- Index

updateAnnotations :: IntermediaryA I -> IntermediaryA_St I
updateAnnotations (as, st) = result
  where
    result = findSolution st g
    g = toAbductionGraph as

findSolution :: IntermediaryA_St I -> G -> IntermediaryA_St I
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
    (_, g') = goStmt (mempty, g) (ab^.aStmt)

type GoStmtData = (SQ.Seq I)
type GoStmtAcc  = (GoStmtData, G)

goStmt :: GoStmtAcc -> StmtA I -> GoStmtAcc
goStmt acc@(implicits, g) s =
  case s of
    Skip                -> acc
    Block{..}           -> foldl' goStmt acc blockStmts
    BlockingAsgn{..}    -> asgn lhs rhs
    NonBlockingAsgn{..} -> asgn lhs rhs
    IfStmt{..}          -> foldl' goStmt (withConds ifCond, g) [thenStmt, elseStmt]
  where
    withConds e = implicits SQ.>< foldVariables e
    asgn l r    = (implicits, goAsgn acc l r)

goAsgn :: GoStmtAcc -> I -> VExprA I -> G
goAsgn (implicits, g) (lName, lIndex) r = g''
  where
    g'  = foldl' (addEdge Implicit) g implicits
    g'' = foldl' (addEdge Direct) g' (foldVariables r)

    addEdge typ g1 (rName, rIndex) =
      if   Gr.hasEdge g1 (rIndex, lIndex)
      then g1
      else Gr.insEdge (rIndex, lIndex, typ) .
           addNode (rIndex, rName) .
           addNode (lIndex, lName) $
           g1

    addNode (ind, name) g1 =
      case Gr.lab g1 ind of
        Nothing -> Gr.insNode (ind, name) g1
        Just _  -> g1

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

instance J.ToJSON EdgeData where
  toJSON Direct   = J.toJSON ("Direct"   :: String)
  toJSON Implicit = J.toJSON ("Implicit" :: String)

instance J.FromJSON EdgeData where
  parseJSON (J.String s) =
    case s of
      "Direct"   -> return Direct
      "Implicit" -> return Implicit
      _          -> fail $ printf "Cannot parse %s into EdgeData" s
  parseJSON _ = fail "Got an error while parsing EdgeData: Expecting a string"
