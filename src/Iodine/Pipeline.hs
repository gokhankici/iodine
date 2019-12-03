{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline (pipeline) where

import           Iodine.Language.IRParser       ( ParsedIR )
import           Iodine.Language.Annotation
import           Iodine.Types
import           Iodine.Transform.Merge
import           Iodine.Transform.Normalize
import           Iodine.Transform.SanityCheck
import           Iodine.Transform.VCGen
import           Iodine.Transform.Query
import           Data.Function
import           Data.Foldable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Trace
import           Text.Printf

{- |
Implements the following pipeline:

IR ----+
       |
Annot ---> SanityCheck -> Merge -> Normalie -> VCGen -> Query
-}
pipeline
  :: Members '[Error IodineException, Trace] r
  => Sem r ParsedIR             -- | parsed ir
  -> Sem r AnnotationFile       -- | parsed annotation file contents
  -> Sem r FInfo                -- | fixpoint query to run
pipeline irReader afReader = do
  ir <- irReader
  af <- afReader
  (do
      sanityCheck & runReader ir
      traceResult "IR" ir
      let mergedIR = merge ir
      traceResult "Merged IR" mergedIR
      ssaOutput@(normalizedIR, _) <- normalize mergedIR
      traceResult "Normalized IR" normalizedIR
      vcgen ssaOutput >>= constructQuery normalizedIR
    )
    & runReader af

traceResult :: (Member Trace r, Show a) => String -> L a -> Sem r ()
traceResult t l = do
  trace (printf "=== %s ===" t)
  traverse_ (trace . toStr) l
  trace sep
  where
    toStr a = show a ++ "\n"
    sep = replicate 80 '-'
