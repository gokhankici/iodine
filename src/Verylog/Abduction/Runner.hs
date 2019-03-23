{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Runner ( runner
                                , runner'
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
                  
