{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Abduction.Types where

import Iodine.Language.Types
import Iodine.Solver.FP.Types

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
import qualified Data.Aeson as J

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

           , _solution    :: Sol     -- solution returned by liquid-fixpoint
           , _isSafe      :: Bool
           , _cost        :: Double  -- the cost of the current annotations & solution
           , _currentFPSt :: FPSt    -- current solution

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

data AbductionAnnot a =
    TagEq    { varName :: a }
  | ValueEq  { varName :: a }
  | TagEq2   { varName :: a, var2Name :: a }
  | ValueEq2 { varName :: a, var2Name :: a }
  | NoTaint  { varName :: a }
  deriving (Eq)

instance Show a => Show (AbductionAnnot a) where
  show (TagEq    {..}) = printf "tag_eq(%s)" (show varName)
  show (ValueEq  {..}) = printf "val_eq(%s)" (show varName)
  show (TagEq2   {..}) = printf "tag_eq(%s, %s)" (show varName) (show var2Name)
  show (ValueEq2 {..}) = printf "val_eq(%s, %s)" (show varName) (show var2Name)
  show (NoTaint  {..}) = printf "no_taint(%s)" (show varName)

instance Hashable a => Hashable (AbductionAnnot a) where
  hashWithSalt n (TagEq v1)       = hashWithSalt n ("te"  :: String, v1)
  hashWithSalt n (ValueEq v1)     = hashWithSalt n ("ve"  :: String, v1)
  hashWithSalt n (TagEq2 v1 v2)   = hashWithSalt n ("te2" :: String, v1, v2)
  hashWithSalt n (ValueEq2 v1 v2) = hashWithSalt n ("te2" :: String, v1, v2)
  hashWithSalt n (NoTaint v1)     = hashWithSalt n ("nt"  :: String, v1)

data EdgeData = Direct
              | Implicit
              deriving (Eq, Show)

data CplexInput =
  CplexInput { cplexEdges      :: [(Int, Int, EdgeData)]
             , cplexMustEq     :: [Int]
             , cplexCannotBeEq :: [Int]
             , cplexMapping    :: [(Int, (Id, Bool))]
             }

instance J.ToJSON CplexInput where
  toJSON CplexInput{..} = J.object [ "edges"        J..= J.toJSON cplexEdges
                                   , "must_eq"      J..= J.toJSON cplexMustEq
                                   , "cannot_be_eq" J..= J.toJSON cplexCannotBeEq
                                   , "mapping"      J..= J.toJSON cplexMapping
                                   ]

instance J.ToJSON EdgeData where
  toJSON Direct   = J.String "Direct"
  toJSON Implicit = J.String "Implicit"

instance J.FromJSON EdgeData where
  parseJSON (J.String s) =
    case s of
      "Direct"   -> return Direct
      "Implicit" -> return Implicit
      _          -> fail $ printf "Cannot parse %s into EdgeData" s
  parseJSON _ = fail "Got an error while parsing EdgeData: Expecting a string"
