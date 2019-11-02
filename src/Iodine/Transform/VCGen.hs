module Iodine.Transform.VCGen
  ( vcgen
  , Horn(..)
  , VCGenOutput
  )
where

import Iodine.Language.Types

import Polysemy
-- import Polysemy.Trace

data Horn a = Nope

type VCGenOutput = L (Horn ())

vcgen :: Sem r VCGenOutput
vcgen = return undefined
