module Verylog.Pipeline ( pipeline
                        , pipeline'
                        ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Transform.Modularize
import Verylog.Transform.Merge
import Verylog.Transform.SanityCheck
import Verylog.Transform.FP.VCGen
import Verylog.Solver.FP.Types

--------------------------------------------------------------------------------
pipeline :: FilePath -> String -> FPSt
--------------------------------------------------------------------------------
pipeline f = common f >>> toFpSt

  
--------------------------------------------------------------------------------
pipeline' :: FilePath -> String -> [AlwaysBlock]
--------------------------------------------------------------------------------
pipeline' f = common f >>> arr fst

common :: FilePath -> String -> ([AlwaysBlock], Annots)
common f = parse f
           >>> first ( flatten >>> sanityCheck )
           >>> merge
