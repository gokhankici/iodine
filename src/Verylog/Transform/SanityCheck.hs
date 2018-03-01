{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.SanityCheck ( sanityCheck
                                     ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.List                  as Li
import qualified Data.HashMap.Strict        as M

import Verylog.Language.Types
-- import Verylog.Transform.Utils

sanityCheck :: [AlwaysBlock] -> [AlwaysBlock]
sanityCheck = id
