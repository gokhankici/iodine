module Verylog.HSFGen ( hsfgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.HSF.Types
import Verylog.Transform.Modularize
import Verylog.Transform.VCGen

pipeline   :: FilePath -> String -> [HSFClause]
pipeline f = parse f >>> modularize >>> invs

hsfgen   :: FilePath -> IO [HSFClause]
hsfgen f = do
  s <- readFile f
  return $ pipeline f s 

