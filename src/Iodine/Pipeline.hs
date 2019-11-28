{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Iodine.Pipeline
  ( pipeline
  )
where

import           Iodine.Language.IRParser       ( ParsedIR
                                                , IRParseError
                                                )
import           Iodine.Language.AnnotationParser
                                                ( AnnotationFile(..) )
import           Iodine.Language.Types
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

type GlobalState r
  = Members
      '[Error SanityCheckError, Error IRParseError, Error VCGenError, Error QueryError, Trace]
      r

pipeline
  :: GlobalState r
  => Id
  -> Sem r ParsedIR
  -> Sem r (AnnotationFile ())
  -> Sem r FInfo
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
