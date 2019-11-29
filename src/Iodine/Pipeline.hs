{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline (pipeline) where

import           Iodine.Language.IRParser       ( ParsedIR )
import           Iodine.Language.AnnotationParser
                                                ( AnnotationFile(..) )
import           Iodine.Types
import           Iodine.Transform.Merge
import           Iodine.Transform.SSA
import           Iodine.Transform.SanityCheck
import           Iodine.Transform.VCGen
import           Iodine.Transform.Query
import           Data.Function
import           Data.Foldable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Trace

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
      traceList ir
      let mergedIR = merge ir
      traceList mergedIR
      ssaOutput@(ssaIR, _) <- ssa mergedIR
      traceList ssaIR
      vcgen ssaOutput >>= constructQuery ssaIR
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

traceList :: (Member Trace r, Show a) => L a -> Sem r ()
traceList l = traverse_ (trace . toStr) l >> trace sep
  where
    toStr a = show a ++ "\n"
    sep = replicate 80 '-'
