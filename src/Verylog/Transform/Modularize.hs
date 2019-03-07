{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize ( flatten
                                    , filterAnnotSt
                                    ) where

import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS

import           Verylog.Language.Types
import           Verylog.Transform.Utils

type States = (St, AnnotSt)

-- | flatten:: Flatten the module hierarchy
-----------------------------------------------------------------------------------
flatten :: States -> [AlwaysBlock]
-----------------------------------------------------------------------------------
flatten input = evalState (m_flattenToAlways input []) 0

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

-- | Keep the keys that are present in the given set
filterMap :: HS.HashSet Id -> UFMap -> UFMap
filterMap toKeep = HM.filterWithKey (\k _ -> HS.member k toKeep)

filterVars :: HS.HashSet Id -> [Var] -> [Var]
filterVars toKeep = filter (\v -> HS.member (varName v) toKeep)

