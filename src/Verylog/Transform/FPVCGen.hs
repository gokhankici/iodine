{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.FPVCGen ( fpInvs
                                 ) where

import           Control.Lens
import qualified Data.HashMap.Strict      as M

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Solver.FP.Types
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

fpInvs    :: [AlwaysBlock] -> [Inv]
fpInvs as = invs as

