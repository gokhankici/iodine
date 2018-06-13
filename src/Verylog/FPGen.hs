module Verylog.FPGen ( pipeline ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen
import Verylog.Solver.FP.Types

--------------------------------------------------------------------------------
pipeline :: FilePath -> String -> FPSt
--------------------------------------------------------------------------------
pipeline f = parse f
             >>> flatten
             >>> sanityCheck
             >>> toFpSt

  
