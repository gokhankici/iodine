{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.RandomSearch (abduction) where

import Prelude hiding (break)

import Verylog.Abduction.Types
import Verylog.Abduction.Parser
import Verylog.Abduction.Utils
import Verylog.Abduction.Sample

import Verylog.Language.Types
import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Utils

import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.Sequence       as SQ
import qualified Data.Foldable       as F
import           System.IO
import           Text.Printf

--------------------------------------------------------------------------------
abduction :: FilePath -> FC.Config -> FPSt -> IO (Bool, Sol)
--------------------------------------------------------------------------------
abduction fn fcConfig st = do
  (isSafe', s') <- runStateT act $
    S { _t           = 1.0
      , _tMin        = 1e-5
      , _alpha       = 0.1
      , _step        = 2
      , _curStep     = 0
      , _isSafe      = False
      , _solution    = mempty
      , _cost        = 0.0
      , _currentFPSt = st
      , _cfg         = fcConfig
      , _globalMd    = mempty
      , _negAnnots   = mempty
      }
  let sol' = s' ^. solution
  return (isSafe', sol')

  where
    act = do
      -- load the good and bad annotations
      readAnnots fn

      (safe, sol) <- runSolve0

      isSafe   .= safe
      solution .= sol
      ast      <- use (currentFPSt.fpAnnotations)
      cost     <~ calculateCost ast (safe, sol)
      globalMd .= collectMd (st^.fpABs)

      if (not safe)
        then do b <- outerLoop
                when b $ writeAnnots fn
                return b
        else return True

    collectMd :: SQ.Seq AlwaysBlock -> GlobalMetadata
    collectMd =
      let f m a = over gmVariables (upd a) m
          upd a = let ws :: SQ.Seq Id = set2seq $ a ^. aMd ^. mRegisters
                      rs :: SQ.Seq Id = set2seq $ a ^. aMd ^. mWires
                  in  (SQ.><) (rs SQ.>< ws)
      in F.foldl' f mempty

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
  ast' <- sample
  (safe', sol') <- runSolve ast'
  cost' <- calculateCost ast' (safe',sol')
  p <- acceptanceProb cost'
  debugM $ printf "acceptance prob = %g" p
  ifM (fmap (p >) randM)
    (updateSol safe' ast' sol' cost')
    (debugM $ printf "skipping solution\n%s" (show ast')
    )
  ifM (use isSafe)
    askToStop
    (continue $ return False)

  where
    askToStop = do
      printGoodAnnot
      c <- yesno "Exit ?"
      if c
        then break $ return True
        else continue $ return False

printGoodAnnot :: M ()
printGoodAnnot =
  use (currentFPSt.fpAnnotations) >>=
  debugM . printf "positive annots:\n%s" . show

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

updateSol :: Bool -> AnnotSt -> Sol -> Double -> M ()
updateSol safe' ast' sol' cost' = do
  isSafe .= safe'
  currentFPSt.fpAnnotations .= ast'
  solution .= sol'
  cost .= cost'

runSolve0 :: M (Bool, Sol)
runSolve0 = app2 (liftIO2 solve) (use cfg) (use currentFPSt)

runSolve :: AnnotSt -> M (Bool, Sol)
runSolve ast = do
  fpst' <- toFpSt' ast <$> use currentFPSt
  (liftIO1 $ flip solve fpst') =<< use cfg

l :: String
l = cp 80 '-'

cp :: Int -> a -> [a]
cp n a = take n $ repeat a

