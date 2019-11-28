{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE StrictData        #-}

module Iodine.Types where

import qualified Data.Sequence as SQ
import qualified Data.Text     as T

type Id = T.Text
type L  = SQ.Seq

data IodineExceptionType =
    IRParser
  | SanityCheck
  | VCGen
  | Query
  deriving (Show, Eq)

data IodineException =
  IE { exceptionSource  :: IodineExceptionType 
     , exceptionMessage :: String
     }

instance Show IodineException where
  show (IE src msg) = show src ++ ": " ++ msg
