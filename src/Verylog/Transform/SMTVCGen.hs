{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.SMTVCGen ( smtInvs
                                  ) where

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Transform.VCGen

smtInvs    :: [AlwaysBlock] -> [Inv]
smtInvs as = invs as

