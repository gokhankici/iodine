{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.HSFVCGen ( hsfInvs
                                  ) where

import           Control.Lens

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Solver.HSF.Types
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

hsfInvs    :: [AlwaysBlock] -> ([QueryNaming], [Inv])
hsfInvs as = (map queryNaming as, invs as)

queryNaming   :: AlwaysBlock -> QueryNaming
queryNaming a = QueryNaming (a^.aId) (makeInvArgs fmt{atomVar=True} a)
