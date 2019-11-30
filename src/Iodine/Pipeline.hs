{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline (pipeline) where

import           Iodine.Language.IRParser       ( ParsedIR )
import           Iodine.Language.AnnotationParser
                                                ( AnnotationFile(..) )
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
Annot ---> SanityCheck -> Merge -> SSA -> VCGen -> Query
-}
pipeline
  :: Members '[Error IodineException, Trace] r
  => Id                         -- | top module name
  -> Sem r ParsedIR             -- | parsed ir
  -> Sem r (AnnotationFile ())  -- | parsed annotation file contents
  -> Sem r FInfo                -- | fixpoint query to run
pipeline topmodule irReader afReader = do
  ir <- irReader
  af <- fixAF <$> afReader
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
 where
    -- TODO: this is currently a hack, annotation file needs to be updated
  fixAF AnnotationFile {..} =
    AnnotationFile
    { afTopModule = if afTopModule == ""
                    then topmodule
                    else afTopModule
    , ..
    }

traceResult :: (Member Trace r, Show a) => String -> L a -> Sem r ()
traceResult t l = do
  trace (printf "=== %s ===" t)
  traverse_ (trace . toStr) l
  trace sep
  where
    toStr a = show a ++ "\n"
    sep = replicate 80 '-'
