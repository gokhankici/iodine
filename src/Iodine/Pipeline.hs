module Iodine.Pipeline (pipeline) where

import Iodine.Language.IRParser
import Iodine.Language.AnnotationParser
import Iodine.Transform.SSA
import Iodine.Transform.SanityCheck

import Control.Arrow
import Control.Monad

import qualified Data.ByteString.Lazy as B

type PipelineInput = ( FilePath -- IR file
                     , FilePath -- Annotation file
                     )
type PipelineOutput = IO Bool

pipeline :: PipelineInput -> PipelineOutput
pipeline (irFile, annotationFile) = do
  result <- transform <$> ((,) <$> (,) irFile <$> readFile irFile <*> B.readFile annotationFile)
  forM_ result print
  return True

transform :: ((FilePath, String), B.ByteString) -> (SSAIR, AnnotationFile ())
transform = (first parse >>> second parseAnnotations) >>>
            sanityCheck >>>
            first ssa
