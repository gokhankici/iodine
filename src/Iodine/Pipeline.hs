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
      let mergedIR = merge ir
      traverse_ (trace . show) ir
      trace "--------------------------------------------------------------------------------"
      traverse_ (trace . show) mergedIR
      trace "--------------------------------------------------------------------------------"
      ssaOutput@(ssaIr, _) <- ssa mergedIR
      traverse_ (trace . show) ssaIr
      vcgen ssaOutput >>= constructQuery ssaIr
    )
    & runReader af

 where
    -- TODO: this is currently a hack, annotation file needs to be updated
  fixAF AnnotationFile {..} = AnnotationFile
    { afTopModule = if afTopModule == "" then topmodule else afTopModule
    , ..
    }
