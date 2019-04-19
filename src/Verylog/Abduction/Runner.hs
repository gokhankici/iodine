{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Runner ( runner
                                , runner'
                                , runner3
                                ) where

import Verylog.Abduction.Graph
import Verylog.Abduction.RandomSearch
import Verylog.Abduction.Types
-- import Verylog.Abduction.Utils

import Verylog.Language.Types
import Verylog.Abduction.Transform
import Verylog.Types
import Verylog.Solver.FP.Types

import qualified Language.Fixpoint.Types.Config as FC

-- import Control.Lens
import Data.Functor.Compose
import Data.Functor.Product
import Data.Sequence

import           Control.Lens
import qualified Data.Aeson               as J
import qualified Data.HashSet             as HS
import qualified Data.Graph.Inductive     as Gr
import qualified Data.IntSet              as IS
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy     as B

--------------------------------------------------------------------------------
runner :: FilePath -> FC.Config -> FPSt -> IO (Bool, Sol)
--------------------------------------------------------------------------------
runner = abduction

--------------------------------------------------------------------------------
runner' :: Intermediary -> Intermediary
--------------------------------------------------------------------------------
runner' input = removeId (as', newAnnots)
  where
    inputWithIndex@(as', _) = giveId input
    newAnnots = updateAnnotations inputWithIndex

runner3 :: Intermediary -> IO ()
runner3 input = do B.writeFile "cplex.json" $ J.encode ci
                   printGraph
  where
    (as, (st, _)) = giveId input
    g             = toAbductionGraph as
    ci            = toCplexInput g st
    printGraph    = B.writeFile "graph.json" $
                    J.encode (Gr.labNodes g, Gr.labEdges g)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

giveId :: IntermediaryA Id -> IntermediaryA (Id, Int)
giveId = fromProduct . giveUniqueId . toProduct

removeId :: IntermediaryA (Id, Int) -> IntermediaryA Id
removeId = fromProduct . undoUniqueId . toProduct

-- MyProduct = (Seq (AlwaysBlock a), (AnnotSt a, [FPQualifier a]))
type MyProduct a = Product (Compose Seq AlwaysBlockA) (Product (AnnotStA) (Compose [] FPQualifierA)) a

toProduct :: IntermediaryA a -> MyProduct a
toProduct (as, (annots, qualifiers)) = Pair (Compose as) (Pair annots (Compose qualifiers))

fromProduct :: MyProduct a -> IntermediaryA a
fromProduct (Pair (Compose as) (Pair annots (Compose qualifiers))) = (as, (annots, qualifiers))

toCplexInput :: G -> AnnotStA (Id, Int) -> CplexInput
toCplexInput g st = CplexInput{..}
  where
    sinksIds        = [ n | (_, n) <- HS.toList $ st ^. sinks ]
    cplexEdges      = [ (u,v) | (u,v,_) <- Gr.labEdges g ]
    cplexMustEq     = go sinksIds
    cplexCannotBeEq = []
    cplexMapping    = Gr.labNodes g

    go :: [Int] -> [Int]
    go is =
      let initSt = ( IS.fromList is -- worklist
                   , ( IS.empty     -- direct seen nodes
                     , IS.empty     -- implicit seen nodes
                     )
                   , IS.empty       -- results
                   )
          loop = do
            w <- use _1         -- get worklist
            if IS.null w
              then return ()
              else do let v = IS.findMin w
                      _1  %= IS.delete v -- remove node from worklist
                      let vParents = Gr.lpre g v
                      forM_ vParents $ \(u, typ) -> do
                        case typ of
                          Direct   -> do c <- uses (_2 . _1) (IS.member u) -- check if seen directly before
                                         if c
                                           then return ()
                                           else do _2 . _1 %= IS.insert u -- mark node as seen directly
                                                   _1      %= IS.insert u -- add node to the workset
                          Implicit -> do c <- uses (_2 . _2) (IS.member u) -- check if seen implicitly before
                                         if c
                                           then return ()
                                           else do _2 . _2 %= IS.insert u -- mark node as seen implicitly
                                                   _3      %= IS.insert u -- add node to the results
                      loop
      in IS.toList $ (execState loop initSt) ^. _3
