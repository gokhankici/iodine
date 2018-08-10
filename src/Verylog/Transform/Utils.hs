{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Utils where

import           Verylog.Language.Types
import           Verylog.Solver.Common

import           Control.Lens
import           Control.Exception
import           Text.Printf
import           Debug.Trace
import           Data.List
import qualified Data.HashSet            as HS

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

isTag :: Id -> Bool
isTag v = isPrefixOf "VLT" v || isPrefixOf "VRT" v

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

makeBothTags :: [Id] -> [Id]
makeBothTags vs = [mk fmt{leftVar=True}, mk fmt{rightVar=True}] <*> vs
  where
    mk f = makeVarName f{taggedVar=True}

trc         :: Show b => String -> b -> a -> a
trc msg b a = trace (printf "%s%s" msg (show b)) a

constants :: [(String,Expr)]
constants = [ ("zero",  Number 0)
            , ("one",   Number 1)
            , ("tru",   Boolean True)
            , ("fals",  Boolean False)
            ]

getConstantName :: Expr -> String
getConstantName e =
  case find ((==) e . snd) constants of
    Just (name,_) -> name
    Nothing       -> throw $ PassError $ printf "unknown constant: %s" (show e)

dbg       :: String -> a -> a
dbg str a = if verbose then trace str a else a
  where
    verbose = False

twoPairs :: [a] -> [(a,a)]
twoPairs []     = []
twoPairs (x:xs) = [ (x,x') | x' <- xs ] ++ twoPairs xs

getLhss :: Stmt -> HS.HashSet Id
getLhss s = h s
  where
    h Skip                  = HS.empty
    h (BlockingAsgn{..})    = HS.singleton lhs
    h (NonBlockingAsgn{..}) = HS.singleton lhs
    h (IfStmt{..})          = foldMap h [thenStmt, elseStmt]
    h (Block{..})           = foldMap h blockStmts
