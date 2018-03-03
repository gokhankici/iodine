module Verylog.HSFGen ( hsfgen ) where

import Control.Arrow

import Verylog.Language.Parser

import Verylog.Solver.Common
import Verylog.Solver.HSF.Types

import Verylog.Transform.Modularize
import Verylog.Transform.SanityCheck
import Verylog.Transform.VCGen

type Out = ([QueryNaming], [Inv])  

pipeline   :: FilePath -> String -> Out
pipeline f = parse f >>> modularize >>> sanityCheck >>> hsfInvs

hsfgen   :: FilePath -> IO Out
hsfgen f = do
  s <- readFile f
  return $ pipeline f s 

