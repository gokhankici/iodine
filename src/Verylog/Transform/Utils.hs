{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Utils where

import           Control.Lens
import           Control.Exception
import           Text.Printf
import           Debug.Trace
import           Data.List

import           Verylog.Language.Types
import           Verylog.Solver.Common

data VarFormat = VarFormat { taggedVar   :: Bool
                           -- , primedVar   :: Bool
                           , leftVar     :: Bool
                           , rightVar    :: Bool
                           , atomVar     :: Bool
                           , varId       :: Maybe Int
                           , paramVar    :: Bool
                           }
                 deriving (Show)

fmt :: VarFormat
fmt = VarFormat { taggedVar   = False
                -- , primedVar   = False
                , leftVar     = False
                , rightVar    = False
                , atomVar     = False
                , varId       = Nothing
                , paramVar    = False
                } 

makeVar :: VarFormat -> Id -> Expr
makeVar f v = Var (makeVarName f v)

makeVarName :: VarFormat -> Id -> Id
makeVarName f@(VarFormat{..}) v =
  -- printf "%s%sV%s%s%s_%s" par atom pos tag vid v
  par ++ atom ++ "V" ++ pos ++ tag ++ vid ++ "_" ++ v

  where
    atom | atomVar   = "v"
         | otherwise = ""

    par  | paramVar  = "arg_"
         | otherwise = ""

    tag  | taggedVar = "T"
         | otherwise = ""

    vid   = maybe "" show varId

    pos   | (leftVar && rightVar) = throw (PassError $ "Both left & right requested from makeVarName for " ++ v ++ " " ++ show f)
          | leftVar   = "L"
          | rightVar  = "R"
          | otherwise = ""

allArgs        :: VarFormat -> St -> [Id]
allArgs f st = let ps = map varName $ filter isRegister (st^.ports)
                 in (makeVarName f <$> ps) ++ (makeVarName f{taggedVar=True} <$> ps)
          
makeInvArgs     :: VarFormat -> AlwaysBlock -> [Id]
makeInvArgs f a = allArgs f{leftVar=True} st ++ allArgs f{rightVar=True} st
  where
    st = a^.aSt

makeInvTags     :: VarFormat -> AlwaysBlock -> [Id]
makeInvTags f a = allTags f{leftVar=True} ++ allTags f{rightVar=True}
  where
    st         = a^.aSt
    rs         = map varName $ filter isRegister (st^.ports)
    allTags f' = makeVarName f'{taggedVar=True} <$> rs

trc         :: Show b => String -> b -> a -> a
trc msg b a = trace (printf "%s%s" msg (show b)) a

constants :: [(String,Integer)]
constants = [ ("zero", 0)
            , ("one",  1)
            ]

getConstantName :: Int -> String
getConstantName n =
  case find ((==) n' . snd) constants of
    Just (name,_) -> name
    Nothing       -> throw $ PassError $ printf "constant %d is not defined (should be 0 or 1)" n
  where
    n' = toInteger n

dbg       :: String -> a -> a
dbg str a = if verbose then trace str a else a
  where
    verbose = False
