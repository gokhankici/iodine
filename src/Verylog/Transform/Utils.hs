{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Transform.Utils where

import           Verylog.Language.Types
import           Verylog.Solver.Common

import           Control.Arrow
import           Control.Lens
import           Control.Exception
import           Text.Printf
import           Debug.Trace
import           Data.Char
import           Data.List
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Sequence as SQ

data VarFormat = VarFormat { taggedVar   :: Bool
                           , leftVar     :: Bool
                           , rightVar    :: Bool
                           , atomVar     :: Bool
                           , varId       :: Maybe Int
                           , paramVar    :: Bool
                           }
                 deriving (Show)

fmt :: VarFormat
fmt = VarFormat { taggedVar = False
                , leftVar   = False
                , rightVar  = False
                , atomVar   = False
                , varId     = Nothing
                , paramVar  = False
                }

makeVar :: VarFormat -> Id -> Expr
makeVar f v = Var (makeVarName f v)

makeVarName :: VarFormat -> Id -> Id
makeVarName f@(VarFormat{..}) v = par +=+ atom +=+ "V" +=+ pos +=+ tag +=+ vid +=+ "_" +=+ v
  where
    (+=+) = idAppend

    atom | atomVar   = "v" :: Id
         | otherwise = "" :: Id

    par  | paramVar  = "arg_" :: Id
         | otherwise = "" :: Id

    tag  | taggedVar = "T" :: Id
         | otherwise = "" :: Id

    -- vid   = maybe "" show varId
    vid   = case varId of
              Nothing -> "" :: Id
              Just n  -> T.pack $ show n


    pos   | (leftVar && rightVar) =
            throw (PassError $ "Both left & right requested from makeVarName for " ++ id2Str v ++ " " ++ show f)
          | leftVar   = "L" :: Id
          | rightVar  = "R" :: Id
          | otherwise = "" :: Id

parseVarName :: Id -> (VarFormat, Id)
parseVarName v = pipeline (fmt, v)
  where
    pipeline = parseParam >>> parseAtom >>> removeV >>> parsePos >>> parseTag >>> parseId

    parseParam = go "arg_" $ \f r -> f { paramVar  = r }
    parseAtom  = go "v"    $ \f r -> f { atomVar   = r }
    parseTag   = go "T"    $ \f r -> f { taggedVar = r }

    removeV    = second $ \s ->
      case T.uncons s of
        Just ('V',rest) -> rest
        _               -> error "expected 'V' in the variable prefix"

    parsePos (f, s) =
      case T.uncons s of
        Just ('L',rest) -> (f { leftVar  = True }, rest)
        Just ('R',rest) -> (f { rightVar = True }, rest)
        _               -> (f, s)

    parseId (f, s) = if | T.null digits -> (f, s')
                        | otherwise     -> (f { varId = read $ id2Str digits }, s')
      where
        (digits, rest) = T.span isDigit s
        s' = case T.uncons rest of
               Just ('_',ss) -> ss
               _             -> error "expected a '_' before the variable name"

    go pfx f (a, s) =
      if   pfx `T.isPrefixOf` s
      then (f a True,  T.drop (T.length pfx) s)
      else (f a False, s)


isTag :: Id -> Bool
-- isTag v = isPrefixOf "VLT" v || isPrefixOf "VRT" v
isTag = taggedVar . fst . parseVarName


allArgs        :: VarFormat -> St -> SQ.Seq Id
allArgs f st = let ps = fmap varName $ SQ.filter isRegister (st^.ports)
                 in (makeVarName f <$> ps) SQ.>< (makeVarName f{taggedVar=True} <$> ps)

makeInvArgs     :: VarFormat -> AlwaysBlock -> SQ.Seq Id
makeInvArgs f a = allArgs f{leftVar=True} st SQ.>< allArgs f{rightVar=True} st
  where
    st = a^.aSt

makeInvTags     :: VarFormat -> AlwaysBlock -> SQ.Seq Id
makeInvTags f a = allTags f{leftVar=True} SQ.>< allTags f{rightVar=True}
  where
    st         = a^.aSt
    rs         = fmap varName $ SQ.filter isRegister (st^.ports)
    allTags f' = makeVarName f'{taggedVar=True} <$> rs


makeBothTag :: Id -> SQ.Seq Id
makeBothTag v = mk fmt{leftVar=True} v  SQ.<|
                mk fmt{rightVar=True} v SQ.<|
                mempty
  where
    mk f = makeVarName f{taggedVar=True}

makeBothTags :: SQ.Seq Id -> SQ.Seq Id
makeBothTags vs = foldl' h mempty vs
  where
    h acc v = mk fmt{leftVar=True} v
              SQ.<| mk fmt{rightVar=True} v
              SQ.<| acc
    mk f = makeVarName f{taggedVar=True}

trc         :: Show b => String -> b -> a -> a
trc msg b a = trace (printf "%s%s" msg (show b)) a

constants :: [(Id,Expr)]
constants = [ ("zero",  Number 0)
            , ("one",   Number 1)
            , ("tru",   Boolean True)
            , ("fals",  Boolean False)
            ]

getConstantName :: Expr -> Id
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
