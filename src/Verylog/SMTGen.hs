module Verylog.SMTGen ( smtgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.SMTVCGen

pipeline f = parse f >>> modularize >>> sanityCheck >>> smtInvs

smtgen f = do
  s <- readFile f
  return $ pipeline f s 
