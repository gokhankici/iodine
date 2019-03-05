{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize ( flatten
                                    , filterAnnotSt
                                    ) where

import           Control.Arrow
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.List (foldl')

import           Verylog.Language.Types
import           Verylog.Solver.FP.Types
import           Verylog.Transform.Utils

type States = (St, AnnotSt)

-- | flatten:: Flatten the module hierarchy
-----------------------------------------------------------------------------------
flatten :: (St, AllAnnots) -> [AlwaysBlock]
-----------------------------------------------------------------------------------
flatten = second toAnnotSt >>> flattenToAlways

flattenToAlways :: States -> [AlwaysBlock]
flattenToAlways input = evalState (m_flattenToAlways input []) 0

type HS = State Int

m_flattenToAlways :: States -> [AlwaysBlock] -> HS [AlwaysBlock]
m_flattenToAlways (st, ast) l = foldM (\as ir -> flattenIR st ir as) l (st^.irs)
  where
    flattenIR :: St -> IR -> [AlwaysBlock] -> HS [AlwaysBlock]
    flattenIR stt (Always{..}) l' = do
      i <- get
      put (i+1)
      let stt' = filterSt alwaysStmt stt
          ast' = filterAnnotSt alwaysStmt stt' ast
          a    = AB event alwaysStmt i stt' ast' alwaysLoc
      return (a:l')
    flattenIR _  (ModuleInst{..}) l' =
      m_flattenToAlways (modInstSt, ast) l'

-- | Filter the ports and the uninterpreted functions of the state
filterSt :: Stmt -> St -> St
filterSt s st =
  over ports (filterVars vars') .
  set  ufs   ufs' .
  set  irs   [] .
  set  vars  vars' $
  st
  where
    vars0 = HS.fromList $ foldVariables s -- all variables used in the statement
    ufs'  = filterMap vars0 (st^.ufs)
    vars' = vars0 `HS.union` (HS.fromList $ concat $ snd <$> HM.elems ufs')

filterAnnotSt :: Stmt -> St -> AnnotSt -> AnnotSt
filterAnnotSt s stt astt =
  over sources  (HS.intersection vs) .
  over assertEq (HS.intersection vs) .
  over sinks    (HS.intersection lhss)  .
  over sanitize (HS.intersection vs) $
  astt
  where
    lhss = getLhss s
    vs   = stt^.vars

filterList :: HS.HashSet Id -> [Id] -> [Id]
filterList toKeep = filter (\x -> HS.member x toKeep)

-- | Keep the keys that are present in the given set
filterMap :: HS.HashSet Id -> UFMap -> UFMap
filterMap toKeep = HM.filterWithKey (\k _ -> HS.member k toKeep)

filterVars :: HS.HashSet Id -> [Var] -> [Var]
filterVars toKeep = filter (\v -> HS.member (varName v) toKeep)

toAnnotSt :: AllAnnots -> AnnotSt
toAnnotSt aas = foldl' (flip go) mempty (aas^.allAnnotations)
  where
    go (Source s)         = over sources (HS.insert s)
    go (Sink s)           = over sinks (HS.insert s)
    go (TaintEq v)        = over taintEq (HS.insert v)
    go (AssertEq v)       = over assertEq (HS.insert v)
    go (Sanitize vs)      = over sanitize (\hs -> foldl' (flip HS.insert) hs vs)
    go (SanitizeGlob v)   = over sanitizeGlob (HS.insert v)
    go (SanitizeMod {..}) = id
