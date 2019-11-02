module Iodine.Transform.Solve
  ( solve
  )
where

import Iodine.Transform.VCGen (VCGenOutput)

import Polysemy
-- import Polysemy.Trace

solve :: VCGenOutput -> Sem r Bool
solve _ = return True
