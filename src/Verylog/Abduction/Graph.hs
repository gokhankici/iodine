{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Abduction.Graph ( updateAnnotations
                               ) where
import Verylog.Abduction.Utils

import Verylog.Types
import Verylog.Language.Types
-- import Verylog.Solver.FP.Types

import           Control.Lens
import qualified Data.HashSet         as HS
import           Data.Foldable
import qualified Data.Graph.Inductive as Gr
import           Data.Sequence
import           GHC.Generics hiding (to)

data EdgeData = Direct
              | Implicit
              deriving (Eq, Generic)

type V = ()
type E = EdgeData
type G = Gr.Gr V E

type I = Int -- Index

updateAnnotations :: IntermediaryA I -> IntermediaryA_2 I
updateAnnotations (as, (annots, qualifiers)) = result
  where
    result = (annots', qualifiers')
    g = toAbductionGraph as

    annots' = annots & sanitizeGlob %~ HS.union (findControlVars g)

    qualifiers' = qualifiers

findControlVars :: G -> HS.HashSet I
findControlVars g = debug (Gr.prettify g) HS.empty

toAbductionGraph :: ABS I -> G
toAbductionGraph as = foldl' goAB Gr.empty as

-- -----------------------------------------------------------------------------
-- Graph Construction
-- -----------------------------------------------------------------------------

goAB :: G -> AlwaysBlockA I -> G
goAB g ab = g'
  where
    (_, g') = goStmt (mempty, g) (ab^.aStmt)

type GoStmtData = (Seq I)
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
    withConds e = implicits >< foldVariables e 
    asgn l r    = (implicits, goAsgn acc l r)

goAsgn :: GoStmtAcc -> I -> VExprA I -> G
goAsgn (implicits, g) lIndex r = g''
  where
    g'  = foldl' (addEdge Implicit) g implicits
    g'' = foldl' (addEdge Direct) g' (foldVariables r)

    addEdge typ g1 rIndex =
      Gr.insEdge (rIndex, lIndex, typ) .
      Gr.insNodes [(rIndex, ()), (lIndex, ())] $
      g1

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

instance Show EdgeData where
  show Direct   = "D"
  show Implicit = "I"
