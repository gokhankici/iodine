module Iodine.Pipeline
  ( pipeline
  )
where

import Iodine.Language.IRParser
import Iodine.Transform.SSA

import Control.Monad

import qualified Data.ByteString.Lazy as B

type PipelineInput = ( FilePath -- IR file
                     , FilePath -- Annotation file
                     )
type PipelineOutput = IO Bool

pipeline :: PipelineInput -> PipelineOutput
pipeline (irFile, _annotationFile) = do
  irFileContents <- readFile irFile
  _annotationFileContents <- B.readFile _annotationFile

  let ir = ssa $ parse (irFile, irFileContents)
  forM_ ir print

  return True
