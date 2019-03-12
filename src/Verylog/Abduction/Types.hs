{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Types where

import Verylog.Language.Types
import Verylog.Solver.FP.Types

import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as HS
import qualified Data.Sequence            as SQ
import           Data.Hashable
import           Data.List (intercalate)
import           Data.Foldable (toList)
import           Text.Printf

type Sol = FT.FixSolution
type M   = StateT S IO

data GlobalMetadata =
  GlobalMetadata { _gmVariables :: SQ.Seq Id
                 }

-- state used in the annotation algorithm
data S = S { _t       :: Double -- temperature
           , _tMin    :: Double -- minimum temperature
           , _alpha   :: Double -- temperature update factor
           , _step    :: Int    -- # steps taken in each temperature
           , _curStep :: Int    -- current step (between 0 and step)

           , _solution :: Sol     -- solution returned by liquid-fixpoint
           , _isSafe   :: Bool
           , _cost     :: Double  -- the cost of the current annotations & solution
           , _fpst     :: FPSt    -- current solution

           , _cfg :: FC.Config

           , _globalMd :: GlobalMetadata

           , _negAnnots :: AnnotSt
           }

makeLenses ''GlobalMetadata
makeLenses ''S

badStDiff :: AnnotSt -> M AnnotSt
badStDiff ast = do
  bast <- use negAnnots
  return $
    over sanitize (`HS.difference` (bast ^. sanitize)) .
    over sanitizeGlob (`HS.difference` (bast ^. sanitizeGlob)) $
    ast

instance Monoid GlobalMetadata where
  mempty = GlobalMetadata SQ.empty
  m1 `mappend` m2 =
    over gmVariables (SQ.>< m2 ^. gmVariables) $
    m1

instance Show GlobalMetadata where
  show gm = printf "GlobalMetadata(vars=[%s])"
            (intercalate ", " (fmap id2Str $ toList (gm^.gmVariables)))

data R = TagEq    { varName :: Id }
       | ValueEq  { varName :: Id }
       | TagEq2   { varName :: Id, var2Name :: Id }
       | ValueEq2 { varName :: Id, var2Name :: Id }
       | NoTaint  { varName :: Id }
       deriving (Eq)

type RS = HS.HashSet R

instance Show R where
  show (TagEq    {..}) = printf "tag_eq(%s)" varName
  show (ValueEq  {..}) = printf "val_eq(%s)" varName
  show (TagEq2   {..}) = printf "tag_eq(%s, %s)" varName var2Name
  show (ValueEq2 {..}) = printf "val_eq(%s, %s)" varName var2Name
  show (NoTaint  {..}) = printf "no_taint(%s)" varName

instance Hashable R where
  hashWithSalt n (TagEq v1)       = hashWithSalt n ("te", v1)
  hashWithSalt n (ValueEq v1)     = hashWithSalt n ("ve", v1)
  hashWithSalt n (TagEq2 v1 v2)   = hashWithSalt n ("te2", v1, v2)
  hashWithSalt n (ValueEq2 v1 v2) = hashWithSalt n ("te2", v1, v2)
  hashWithSalt n (NoTaint v1)     = hashWithSalt n ("nt", v1)
