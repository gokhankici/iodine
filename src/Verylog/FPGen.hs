module Verylog.FPGen ( fpgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.FPVCGen

pipeline f = parse f >>> modularize >>> sanityCheck >>> fpInvs

fpgen f = do
  s <- readFile f
  return $ pipeline f s 
