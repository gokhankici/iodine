module Abduction (abduction) where

-- import Verylog.MainCommon
-- import Verylog.FPGen
import Verylog.Solver.FP.Types
-- import Verylog.Solver.Common
-- import Verylog.Language.Types
import Verylog.Solver.FP.FQ

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

-- import qualified Data.Set        as S
-- import qualified Data.Map.Strict as M
-- import           Data.List
-- import           Data.Maybe
import           Control.Monad
-- import           Control.Exception
-- import           Control.Lens hiding ((<.>))
-- import           System.Console.ANSI
-- import           System.Console.GetOpt
-- import           System.Environment (getArgs)
-- import           System.FilePath.Posix
import           System.Exit
-- import           System.IO
-- import           Text.PrettyPrint
-- import           Text.Printf

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

