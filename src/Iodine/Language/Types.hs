{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE StrictData        #-}

module Iodine.Language.Types where

import qualified Data.Sequence as SQ
import qualified Data.Text     as T

type Id = T.Text
type L  = SQ.Seq
