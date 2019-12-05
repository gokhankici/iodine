{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline (pipeline) where

import Data.Foldable
import Data.Function
import Iodine.Language.Annotation
import Iodine.Language.IRParser ( ParsedIR )
import Iodine.Transform.Merge
import Iodine.Transform.Normalize
import Iodine.Transform.Query
import Iodine.Transform.SanityCheck
import Iodine.Transform.VCGen
import Iodine.Types
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Trace
import Text.Printf

{- |
Implements the following pipeline:

IR ----+
       |
Annot ---> SanityCheck -> Merge -> Normalie -> VCGen -> Query
-}
pipeline
  :: Members '[Error IodineException, Trace, Output String] r
  => AnnotationFile             -- ^ annotation file
  -> Sem r ParsedIR             -- ^ ir parser
  -> Sem r FInfo                -- ^ fixpoint query to run
pipeline af irReader = do
  ir <- irReader
  runReader af $ do
    sanityCheck & runReader ir
    traceResult "IR" ir
    let mergedIR = merge ir
    traceResult "Merged IR" mergedIR
    ssaOutput@(normalizedIR, _) <- normalize mergedIR
    traceResult "Normalized IR" normalizedIR
    vcgen ssaOutput >>= constructQuery normalizedIR

traceResult :: (Member Trace r, Show a) => String -> L a -> Sem r ()
traceResult t l = do
  trace (printf "=== %s ===" t)
  traverse_ (trace . toStr) l
  trace sep
  where
    toStr a = show a ++ "\n"
    sep = replicate 80 '-'
