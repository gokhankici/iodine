{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Sample ( sample
                                , calculateCost
                                , acceptanceProb
                                ) where

import Prelude hiding (break)

import Verylog.Abduction.Types
import Verylog.Abduction.Utils

import Verylog.Solver.FP.Types
import Verylog.Language.Types

import           Control.Lens
import qualified Data.HashSet        as HS
import           Data.List (foldl')
-- import           Data.Sequence

--------------------------------------------------------------------------------
sample :: M AnnotSt
--------------------------------------------------------------------------------
sample = do
  use (globalMd.gmVariables)
    >>=
    randomSample
    >>=
    randomVar
    >>=
    badStDiff
  where
    randomVar :: Id -> M AnnotSt
    randomVar v =
      chooseM
      (over sanitize     (HS.insert v) <$> use (currentFPSt . fpAnnotations))
      (over sanitizeGlob (HS.insert v) <$> use (currentFPSt . fpAnnotations))

--------------------------------------------------------------------------------
acceptanceProb :: Double -> M Double
--------------------------------------------------------------------------------
acceptanceProb newCost = do
  oldCost     <- use cost
  currentTemp <- use t
  return $ exp ( (oldCost - newCost) / currentTemp )


--------------------------------------------------------------------------------
calculateCost :: AnnotSt -> (Bool, Sol) -> M Double
--------------------------------------------------------------------------------
calculateCost newAnnots (safe, _) = do
  snks <- use (currentFPSt.fpAnnotations.sinks)

  let extraCost1 =
        let f c v = if | v `HS.member` ies -> c + initEqCost   * 10.0
                       | v `HS.member` aes -> c + alwaysEqCost ^ (2::Int)
                       | otherwise         -> c
        in foldl' f 0.0 snks

  return $ if   safe
           then 0.0
           else initEqTotalCost + alwaysEqTotalCost + extraCost1

  where
    initEqCost        = 1.0 :: Double
    alwaysEqCost      = 100.0 :: Double
    initEqTotalCost   = sz ies * initEqCost
    alwaysEqTotalCost = sz aes * alwaysEqCost

    sz :: HS.HashSet a -> Double
    sz = fromIntegral . HS.size

    ies = newAnnots ^. sanitize
    aes = newAnnots ^. sanitizeGlob
