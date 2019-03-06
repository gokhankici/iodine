module Verylog.Abduction (abduction) where

import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types

import qualified Language.Fixpoint.Types.Config as FC

-- import Control.Monad
-- import System.Exit

--------------------------------------------------------------------------------
abduction :: FC.Config -> FPSt -> IO ()
--------------------------------------------------------------------------------
abduction cfg fpst = do
  res <- solve cfg fpst
  return ()


