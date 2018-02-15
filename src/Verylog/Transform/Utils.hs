{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.Utils where

import           Control.Lens
import           Control.Monad.Reader
-- import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
import           Control.Exception
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.HSF.Types

isUF   :: Id -> Reader St Bool
isUF v = views ufs (M.member v)

data VarFormat = VarFormat { taggedVar :: Bool
                           , primedVar :: Bool
                           , leftVar   :: Bool
                           , rightVar  :: Bool
                           , atomVar   :: Bool
                           }

fmt = VarFormat { taggedVar = False
                , primedVar = False
                , leftVar   = False
                , rightVar  = False
                , atomVar   = False
                } 

makeVarName :: VarFormat -> Id -> HSFVar
makeVarName (VarFormat{..}) v = printf "%sV%s%s%s_%s" atom pos tag prime v
  where
    atom | atomVar   = "v"
         | otherwise = ""

    tag  | taggedVar = "T"
         | otherwise = ""

    prime | primedVar = "1"
          | otherwise = ""

    pos   | leftVar             = "L"
          | rightVar            = "R"
          | leftVar && rightVar = throw (PassError "Both left & right requested from makeVarName")
          | otherwise           = ""
     
