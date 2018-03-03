{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Utils where

import           Control.Lens
import           Control.Exception
import           Text.Printf
import           Debug.Trace
import           Data.Char
import           Data.List

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Solver.HSF.Types

data VarFormat = VarFormat { taggedVar   :: Bool
                           , primedVar   :: Bool
                           , leftVar     :: Bool
                           , rightVar    :: Bool
                           , atomVar     :: Bool
                           , varId       :: Maybe Int
                           }
                 deriving (Show)

fmt = VarFormat { taggedVar   = False
                , primedVar   = False
                , leftVar     = False
                , rightVar    = False
                , atomVar     = False
                , varId       = Nothing
                } 

-- set this variable to True if you want to "simplify" the variables
-- inside the HSF file
debugSimple = True -- False

makeVar :: VarFormat -> Id -> Expr
makeVar fmt v = Var (makeVarName fmt v)

makeVarName :: VarFormat -> Id -> HSFVar
makeVarName fmt@(VarFormat{..}) v =
  if   debugSimple
  then let v' = if isPrefixOf "v_" v
                then case drop 2 v of
                       []  -> throw (PassError $ printf "weird variable %s" v)
                       h:t -> (toUpper h):t
                else case v of
                       []  -> throw (PassError "empty var")
                       h:t -> (toUpper h):t
       in printf "%s%s%s%s%s%s" atom v' pos vid prime tag 
  else printf "%sV%s%s%s%s_%s" atom pos tag prime vid v

  where
    atom | atomVar   = "v"
         | otherwise = ""

    tag  | debugSimple && taggedVar = "_t"
         | taggedVar = "T"
         | otherwise = ""

    prime | debugSimple && primedVar = "p"
          | primedVar = "P"
          | otherwise = ""

    vid   = maybe "" show varId

    pos   | (leftVar && rightVar) = throw (PassError $ "Both left & right requested from makeVarName for " ++ v ++ " " ++ show fmt)
          | debugSimple && leftVar   = "l"
          | debugSimple && rightVar  = "r"
          | leftVar   = "L"
          | rightVar  = "R"
          | otherwise = ""

allArgs        :: VarFormat -> St -> [Id]
allArgs fmt st = let ps = st^.ports
                 in (makeVarName fmt                    <$> ps)
                    ++ (makeVarName fmt{taggedVar=True} <$> ps)


          
nextArgs        :: VarFormat -> St -> [Id]
nextArgs fmt st = allArgs fmt st ++ allArgs fmt{primedVar=True} st

makeInvArgs        :: VarFormat -> AlwaysBlock -> [Id]
makeInvArgs fmt a = allArgs fmt{leftVar=True} st ++ allArgs fmt{rightVar=True} st
  where
    st = a^.aSt

trc         :: Show b => String -> b -> a -> a
trc msg b a = trace (printf "%s%s" msg (show b)) a
