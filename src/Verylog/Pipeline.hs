module Verylog.Pipeline ( pipeline
                        , pipeline'
                        ) where

import Control.Arrow

import Verylog.Types

import Verylog.Language.Parser
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Transform.Merge
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck

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
pipeline' :: ParseInput -> Intermediary
--------------------------------------------------------------------------------
pipeline' = common

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
