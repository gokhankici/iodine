module Verylog.FPGen ( pipeline, pipeline' ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen
import Verylog.Solver.FP.Types

--------------------------------------------------------------------------------
pipeline :: FilePath -> String -> FPSt
--------------------------------------------------------------------------------
pipeline f = parse f
             >>> first flatten
             >>> first sanityCheck
             >>> toFpSt

  
--------------------------------------------------------------------------------
pipeline' :: FilePath -> String -> [AlwaysBlock]
--------------------------------------------------------------------------------
pipeline' f = parse f
              >>> arr fst
              >>> flatten
              >>> sanityCheck
