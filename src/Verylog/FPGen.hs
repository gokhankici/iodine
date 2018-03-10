module Verylog.FPGen ( fpgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen
import Verylog.Solver.FP.FQ

pipeline f = parse f >>> modularize >>> sanityCheck >>> fpInvs

fpgen f = do
  s <- readFile f
  let fpst = pipeline f s 
  return $ toFqFormat fpst
  
