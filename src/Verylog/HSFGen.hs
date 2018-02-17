module Verylog.HSFGen ( hsfgen ) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.HSF.Types
import Verylog.Transform.TransitionRelation
import Verylog.Transform.VCGen

pipeline   :: FilePath -> String -> (St, [HSFClause])
pipeline f = parse f
             >>> arr (\st -> (st, []))
             >>> collect next
             >>> collect invs

collect :: (St -> (St, [HSFClause])) -> ((St, [HSFClause]) -> (St, [HSFClause]))
collect step = first step >>> arr (\((st,cs2), cs1) -> (st, cs1 ++ cs2))

hsfgen   :: FilePath -> IO (St, [HSFClause])
hsfgen f = do
  s <- readFile f
  return $ pipeline f s 

