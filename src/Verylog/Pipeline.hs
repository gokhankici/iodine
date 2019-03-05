module Verylog.Pipeline ( pipeline
                        , pipeline'
                        , afterParse
                        , PipelineIntermediary
                        ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Transform.Merge
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck

type PipelineIntermediary = ([AlwaysBlock], AllAnnots)

--------------------------------------------------------------------------------
pipeline :: ParseInput -> FPSt
--------------------------------------------------------------------------------
pipeline = common >>> toFpSt

--------------------------------------------------------------------------------
pipeline' :: ParseInput -> [AlwaysBlock]
--------------------------------------------------------------------------------
pipeline' = common >>> arr fst

common :: ParseInput -> PipelineIntermediary
common = parse >>> afterParse

--------------------------------------------------------------------------------
afterParse :: ParseOutput -> PipelineIntermediary
--------------------------------------------------------------------------------
afterParse = (flatten &&& snd)
             >>> first sanityCheck
             >>> merge

