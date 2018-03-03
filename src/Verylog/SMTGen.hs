module Verylog.SMTGen ( smtgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Solver.Common
-- import Verylog.Solver.SMT.Types
import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.SMTVCGen

pipeline   :: FilePath -> String -> [Inv]
pipeline f = parse f >>> modularize >>> sanityCheck >>> smtInvs

smtgen   :: FilePath -> IO [Inv]
smtgen f = do
  s <- readFile f
  return $ pipeline f s 
