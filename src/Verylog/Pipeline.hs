module Verylog.Pipeline ( pipeline
                        , pipeline'
                        , pipelineWithState
                        ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Transform.Merge
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck

type States       = (St, AnnotSt)
type Qualifiers   = [FPQualifier]
type Intermediary = (ABS, (AnnotSt, Qualifiers))
type ParseInput   = (FilePath, String)
type ParseOutput  = (States, Qualifiers)
type ABS          = [AlwaysBlock]

-- parse       :: (FilePath, String) -> (States, Qualifiers)
-- flatten     :: States -> ABS
-- sanityCheck :: ABS -> ABS
-- merge       :: (ABS, Qualifiers) -> (ABS, Qualifiers)
-- toFpSt      :: (ABS, (AnnotSt, Qualifiers)) -> FPSt

--------------------------------------------------------------------------------
pipeline :: ParseInput -> FPSt
--------------------------------------------------------------------------------
pipeline = common >>> toFpSt

--------------------------------------------------------------------------------
pipeline' :: ParseInput -> [AlwaysBlock]
--------------------------------------------------------------------------------
pipeline' = common >>> fst

--------------------------------------------------------------------------------
pipelineWithState :: ParseInput -> (FPSt, Intermediary)
--------------------------------------------------------------------------------
pipelineWithState = common >>> (toFpSt &&& id)

common :: ParseInput -> Intermediary
common = parse >>> afterParse

afterParse :: ParseOutput -> Intermediary
afterParse = ( (fst >>> flatten)          -- ABS
               &&&
               first snd                  -- (AnnotSt, Qualifiers)
             )
             >>> first sanityCheck        -- (ABS, (AnnotSt, Qualifiers))
             >>> ( (second snd >>> merge) -- ABS
                   &&&
                   snd                    -- (AnnotSt, Qualifiers)
                 )
