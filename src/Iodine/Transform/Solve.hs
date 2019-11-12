{-# LANGUAGE ConstraintKinds #-}

module Iodine.Transform.Solve
  ( solve
  )
where

import Iodine.Transform.VCGen (VCGenOutput)

import Data.Foldable
import Polysemy
import Polysemy.Trace

solve :: FD r => VCGenOutput -> Sem r Bool
solve out = traverse_ (trace . show) out >> return True

type FD r = Members '[ Trace ] r