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
import Verylog.Transform.FP.VCGen
import Verylog.Language.Types

import           Control.Lens
import qualified Data.HashSet        as HS
import           Data.List (foldl')

--------------------------------------------------------------------------------
sample :: M FPSt
--------------------------------------------------------------------------------
sample = do
  ast' <- use (globalMd.gmVariables)
          >>=
          randomSample
          >>=
          randomVar
          >>=
          badStDiff
  toFpSt' ast' <$> use fpst
  where
    randomVar :: Id -> M AnnotSt
    randomVar v =
      chooseM
      (over sanitize     (HS.insert v) <$> use (fpst . fpAnnotations))
      (over sanitizeGlob (HS.insert v) <$> use (fpst . fpAnnotations))

--------------------------------------------------------------------------------
acceptanceProb :: Double -> M Double
--------------------------------------------------------------------------------
acceptanceProb newCost = do
  oldCost     <- use cost
  currentTemp <- use t
  return $ exp ( (oldCost - newCost) / currentTemp )


--------------------------------------------------------------------------------
calculateCost :: FPSt -> (Bool, Sol) -> Double
--------------------------------------------------------------------------------
calculateCost st (safe, _) =
  if   safe
  then 0.0
  else costs

  where
    costs = initEqTotalCost + alwaysEqTotalCost + extraCost1

    initEqCost        = 1.0 :: Double
    alwaysEqCost      = 100.0 :: Double
    initEqTotalCost   = sz ies * initEqCost
    alwaysEqTotalCost = sz aes * alwaysEqCost

    sz :: HS.HashSet a -> Double
    sz = fromIntegral . HS.size

    extraCost1 :: Double
    extraCost1 =
      let f c v = if | v `HS.member` ies -> c + initEqCost   * 10.0
                     | v `HS.member` aes -> c + alwaysEqCost ^ (2::Int)
                     | otherwise         -> c
      in foldl' f 0.0 (ast^.sinks)

    ast = st ^. fpAnnotations
    ies = ast ^. sanitize
    aes = ast ^. sanitizeGlob
