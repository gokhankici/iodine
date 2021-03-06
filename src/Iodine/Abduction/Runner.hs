{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Iodine.Abduction.Runner ( runner
                               ) where

import Iodine.Abduction.Graph
import Iodine.Abduction.Transform
import Iodine.Abduction.Types
import Iodine.Language.Types
import Iodine.Solver.FP.Types
import Iodine.Types

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Functor.Compose
import           Data.Functor.Product
import           Data.Sequence
import qualified Data.Aeson               as J
import qualified Data.ByteString.Lazy     as B
import qualified Data.Graph.Inductive     as Gr
import qualified Data.HashSet             as HS
import qualified Data.IntSet              as IS

--------------------------------------------------------------------------------
runner :: Intermediary -> IO ()
--------------------------------------------------------------------------------
runner input = B.writeFile "cplex.json" $ J.encode ci
  where
    (as, (st, _)) = giveId input
    sinkIds       = [n | (_, n) <- HS.toList (st^.sinks)]
    g             = toAbductionGraph as
    ci            = toCplexInput g sinkIds
    -- printGraph    = B.writeFile "graph.json" $ J.encode (Gr.labNodes g, Gr.labEdges g)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

giveId :: IntermediaryA Id -> IntermediaryA (Id, Int)
giveId = fromProduct . giveUniqueId . toProduct

-- MyProduct = (Seq (AlwaysBlock a), (AnnotSt a, [FPQualifier a]))
type MyProduct a = Product (Compose Seq AlwaysBlockA) (Product AnnotStA (Compose Seq FPQualifierA)) a

toProduct :: IntermediaryA a -> MyProduct a
toProduct (as, (annots, qualifiers)) = Pair (Compose as) (Pair annots (Compose qualifiers))

fromProduct :: MyProduct a -> IntermediaryA a
fromProduct (Pair (Compose as) (Pair annots (Compose qualifiers))) = (as, (annots, qualifiers))

toCplexInput :: G -> [Int] -> CplexInput
toCplexInput g sinkIds = CplexInput{..}
  where
    cplexEdges      = Gr.labEdges g
    cplexMustEq     = cplexToMark g sinkIds
    cplexCannotBeEq = []
    cplexMapping    = Gr.labNodes g

cplexToMark :: G -> [Int] -> [Int]
cplexToMark g is = IS.toList $ execState loop initSt ^. _3
  where
    initSt = ( IS.fromList is -- worklist
             , ( IS.empty     -- direct seen nodes
               , IS.empty     -- implicit seen nodes
               )
             , IS.empty   -- results
             )
    loop = do
      w <- use _1         -- get worklist
      if IS.null w
        then return ()
        else do let v = IS.findMin w
                _1  %= IS.delete v -- remove node from worklist
                let vParents = Gr.lpre g v
                forM_ vParents $ \(u, EdgeData{..}) ->
                  case edgeType of
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
