module Verylog.FPGen ( fpgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen
import Verylog.Solver.FP.FQ
import Verylog.Solver.FP.Types
import Language.Fixpoint.Types

--------------------------------------------------------------------------------
pipeline :: FilePath -> String -> FPSt
--------------------------------------------------------------------------------
pipeline f = parse f >>> modularize >>> sanityCheck >>> toFpSt

--------------------------------------------------------------------------------
fpgen :: FilePath -> IO (FPSt, GInfo SubC Metadata)
--------------------------------------------------------------------------------
fpgen f = do
  s <- readFile f
  let fpst = pipeline f s 
  return (fpst, toFqFormat fpst)
  
