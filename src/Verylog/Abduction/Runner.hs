{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Runner (abduction) where

import Prelude hiding (break)

import Verylog.Abduction.Types
import Verylog.Abduction.Parser
import Verylog.Abduction.Utils
import Verylog.Abduction.Sample

import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Language.Types
import Verylog.Utils

import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashSet        as HS
import qualified Data.Sequence       as SQ
import           Data.List (foldl')
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
abduction :: FilePath -> FC.Config -> FPSt -> IO (Bool, Sol)
--------------------------------------------------------------------------------
abduction fn fcConfig st = do
  (isSafe', s') <- runStateT act $
    S { _t         = 1.0
      , _tMin      = 1e-5
      , _alpha     = 0.1
      , _step      = 2
      , _curStep   = 0
      , _isSafe    = False
      , _solution  = mempty
      , _cost      = 0.0
      , _fpst      = st
      , _cfg       = fcConfig
      , _globalMd  = mempty
      , _negAnnots = mempty
      }
  let sol' = s' ^. solution
  return (isSafe', sol')

  where
    act = do
      -- load the good and bad annotations
      readAnnots fn

      (safe, sol) <- use fpst >>= runSolve

      isSafe   .= safe
      solution .= sol
      cost     .= calculateCost st (safe, sol)
      globalMd .= collectMd (st^.fpABs)

      if (not safe)
        then do b <- outerLoop
                when b $ writeAnnots fn
                return b
        else return True

    collectMd :: [AlwaysBlock] -> GlobalMetadata
    collectMd =
      let f m a = over gmVariables (upd a) m
          upd a = let ws = s2sq $ a ^. aMd ^. mRegisters
                      rs = s2sq $ a ^. aMd ^. mWires
                  in  (SQ.><) (rs SQ.>< ws)
      in foldl' f mempty

    s2sq :: HS.HashSet a -> SQ.Seq a
    s2sq = HS.foldl' (SQ.|>) SQ.empty

outerLoop :: M Bool
outerLoop = while False ((>) <$> use t <*> use tMin) $ do
  curStep .= 0
  safe <- innerLoop
  if safe
    then break $ return True
    else continue $ do a <- use alpha
                       t *= a
                       return False

innerLoop :: M Bool
innerLoop = while False ((>) <$> use step <*> use curStep) $ do
  debug l $ curStep += 1
  fpst' <- sample
  (safe', sol') <- runSolve fpst'
  let cost'  = calculateCost fpst' (safe',sol')
  p <- acceptanceProb cost'
  debugM $ printf "acceptance prob = %g" p
  ifM (fmap (p >) randM)
    (updateSol safe' fpst' sol' cost')
    (debugM $ printf "skipping solution\n%s" (show $ fpst'^.fpAnnotations)
    )
  ifM (use isSafe)
    askToStop
    (continue $ return False)

askToStop :: M (Loop, Bool)
askToStop = do
  printGoodAnnot
  c <- yesno "Exit ?"
  if c
    then break $ return True
    else continue $ return False

printGoodAnnot :: M ()
printGoodAnnot =
  use (fpst.fpAnnotations) >>=
  debugM . printf "pos annots:\n%s" . show

yesno :: MonadIO m => String -> m Bool
yesno prompt = do
          liftIO $ putStr $ prompt ++ " [y/n]: "
          liftIO $ hFlush stdout
          str <- liftIO getLine
          case str of
            "y" -> return True
            "n" -> return False
            _   -> do
              liftIO $ putStrLn "Invalid input."
              yesno prompt

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

updateSol :: Bool -> FPSt -> Sol -> Double -> M ()
updateSol safe' fpst' sol' cost' = do
  isSafe   .= safe'
  fpst     .= fpst'
  solution .= sol'
  cost     .= cost'

runSolve :: FPSt -> M (Bool, Sol)
runSolve f = do
  (liftIO1 $ flip solve f) =<< use cfg

l :: String
l = cp 80 '-'

cp :: Int -> a -> [a]
cp n a = take n $ repeat a

