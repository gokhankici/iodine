{-# LANGUAGE ConstraintKinds #-}

module Iodine.Pipeline
  ( pipeline
  )
where

import Iodine.Language.IRParser (ParsedIR, IRParseError)
import Iodine.Language.AnnotationParser (AnnotationFile)
import Iodine.Transform.SSA
import Iodine.Transform.SanityCheck
import Iodine.Transform.VCGen
import Iodine.Transform.Solve

import Data.Function
import Data.Foldable

import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Trace

type GlobalState r = Members '[ Error SanityCheckError
                              , Error IRParseError
                              , Error VCGenError
                              , Trace
                              ] r

pipeline :: GlobalState r
         => Sem r ParsedIR
         -> Sem r (AnnotationFile ())
         -> Sem r Bool
pipeline irReader afReader = do
  ir <- irReader
  af <- afReader
  (do sanityCheck & runReader ir
      ssaOutput@(ssaIr, _) <- ssa ir
      traverse_ (trace . show) ssaIr
      vcgen ssaOutput >>= solve
    ) & runReader af
