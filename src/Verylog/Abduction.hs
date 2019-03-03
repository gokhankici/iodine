module Verylog.Abduction (abduction) where

import Verylog.Solver.FP.Types
import Verylog.Solver.FP.FQ

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import Control.Monad
import System.Exit

--------------------------------------------------------------------------------
abduction :: Config -> FPSt -> IO ()
--------------------------------------------------------------------------------
abduction cfg fpst = do
  let finfo = toFqFormat fpst
  res <- solve cfg finfo
  printAnnots res

  when (isUnsafe res) $ do
    askToContinue
    let fpst' = fpst
    abduction cfg fpst'

  where
    printAnnots r = do putStr "Result: '"
                       putStr $ show r
                       putStrLn "'"
    askToContinue = do putStrLn "Continue? [y/n]:"
                       response <- getLine
                       when (response /= "y") exitFailure

