module Verylog.VCGen (vcgen) where

import Control.Arrow

import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.HSF.Types
import Verylog.Transform.InitialPass
import Verylog.Transform.TransitionRelation

pipeline f = parse f
             >>> initialPass >>> arr (\st -> (st, []))
             >>> collect next
             >>> arr (\(_st, cs) -> cs)

collect :: (St -> (St, [HSFClause])) -> ((St, [HSFClause]) -> (St, [HSFClause]))
collect step = first step >>> arr (\((st,cs2), cs1) -> (st, cs1 ++ cs2))

vcgen :: FilePath -> IO [HSFClause]
vcgen f = do
  s <- readFile f
  return $ pipeline f s

