{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline (pipeline) where

import           Iodine.Analyze.ModuleSummary
import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Transform.Merge
import           Iodine.Transform.Normalize
import           Iodine.Transform.Query
import           Iodine.Transform.SanityCheck
import           Iodine.Transform.VCGen
import           Iodine.Types

import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.Trace
import           Text.Printf


{- |
Implements the following pipeline:

IR ----+
       |
Annot ---> SanityCheck -> Merge -> Normalize -> VCGen -> Query
-}
pipeline
  :: Members '[Error IodineException, Trace, Output String] r
  => AnnotationFile             -- ^ annotation file
  -> Sem r (L (Module ()))      -- ^ ir parser
  -> Sem r FInfo                -- ^ fixpoint query to run
pipeline af irReader = do
  ir <- irReader
  runReader af $ do
    sanityCheck & runReader ir
    traceResult "IR" ir

    let moduleMap =
          foldl' (\acc m@Module{..} -> HM.insert moduleName m acc) mempty ir
    summaryMap <- createModuleSummaries moduleMap

    runReader summaryMap $ runReader moduleMap $ do
      mergedIR <- merge ir
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
