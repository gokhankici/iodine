{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.Utils where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.HashMap.Strict      as M
import           Control.Exception
import           Text.Printf
import           Debug.Trace

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
                 deriving (Show)

fmt = VarFormat { taggedVar = False
                , primedVar = False
                , leftVar   = False
                , rightVar  = False
                , atomVar   = False
                } 

makeVar :: VarFormat -> Id -> HSFExpr
makeVar fmt v = Var (makeVarName fmt v)

makeVarName :: VarFormat -> Id -> HSFVar
makeVarName fmt@(VarFormat{..}) v = printf "%sV%s%s%s_%s" atom pos tag prime v
  where
    atom | atomVar   = "v"
         | otherwise = ""

    tag  | taggedVar = "T"
         | otherwise = ""

    prime | primedVar = "1"
          | otherwise = ""

    pos   | (leftVar && rightVar) = throw (PassError $ "Both left & right requested from makeVarName for " ++ v ++ " " ++ show fmt)
          | leftVar               = "L"
          | rightVar              = "R"
          | otherwise             = ""

allArgs        :: VarFormat -> St -> [Id]
allArgs fmt st = let ps = st^.ports
                     -- us = st^.ufs.to M.keys
                     -- os = us ++ [done_atom]
                 in (makeVarName fmt                    <$> ps)
                    ++ (makeVarName fmt{taggedVar=True} <$> ps)
                    -- ++ (makeVarName fmt                 <$> os)


          
nextArgs        :: VarFormat -> St -> [Id]
nextArgs fmt st = allArgs fmt st ++ allArgs fmt{primedVar=True} st

invArgs        :: VarFormat -> AlwaysBlock -> [Id]
invArgs fmt a = allArgs fmt{leftVar=True} st ++ allArgs fmt{rightVar=True} st
  where
    st = a^.aSt

trc         :: Show b => String -> b -> a -> a
trc msg b a = trace (printf "%s: %s" msg (show b)) a
