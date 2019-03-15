module Verylog.Pipeline ( pipeline
                        , pipeline'
                        ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Transform.Merge
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck

import Data.Sequence

type ABS          = Seq AlwaysBlock
type States       = (St, AnnotSt)
type Qualifiers   = [FPQualifier]
type Intermediary = (ABS, (AnnotSt, Qualifiers))
type ParseInput   = (FilePath, String)
type ParseOutput  = (States, Qualifiers)

-- parse       :: (FilePath, String) -> (States, Qualifiers)
-- modularize  :: States -> ABS
-- sanityCheck :: ABS -> ABS
-- merge       :: (ABS, Qualifiers) -> (ABS, Qualifiers)
-- toFpSt      :: (ABS, (AnnotSt, Qualifiers)) -> FPSt

-- parse --> modularize --> sanityCheck --> merge --> toFpSt --> liquid-fixpoint

--------------------------------------------------------------------------------
pipeline :: ParseInput -> FPSt
--------------------------------------------------------------------------------
pipeline = common >>> toFpSt

--------------------------------------------------------------------------------
pipeline' :: ParseInput -> ABS
--------------------------------------------------------------------------------
pipeline' = common >>> fst

common :: ParseInput -> Intermediary
common = parse >>> afterParse

afterParse :: ParseOutput -> Intermediary
afterParse = ( (fst >>> modularize)       -- ABS
               &&&
               first snd                  -- (AnnotSt, Qualifiers)
             )
             >>> first sanityCheck        -- (ABS, (AnnotSt, Qualifiers))
             >>> ( (second snd >>> merge) -- ABS
                   &&&
                   snd                    -- (AnnotSt, Qualifiers)
                 )
