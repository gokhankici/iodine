{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Iodine.Language.Types where

-- import           Control.Exception
-- import           Control.Lens
-- import           Control.Monad.Reader
-- import           Control.Monad.State.Lazy
-- import qualified Data.HashSet             as S
import qualified Data.Text                as T
-- import           Data.Typeable
-- import           Text.PrettyPrint
-- import           Data.List
-- import qualified Data.Semigroup as SG
-- import           Data.Hashable
-- import qualified Data.Monoid as Mo
-- import           GHC.Generics hiding (to)
-- import           Control.DeepSeq
-- import qualified Data.Yaml  as Y
import qualified Data.Sequence as SQ
-- import           Data.Foldable

type Id = T.Text
type L  = SQ.Seq
