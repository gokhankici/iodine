{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize ( flatten
                                    ) where

import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS

import Verylog.Language.Types

type States = (St, AnnotSt)

-- | Flatten the module hierarchy
flatten :: States -> [AlwaysBlock]
flatten input = evalState (m_flattenToAlways input []) 0

type HS = State Int

m_flattenToAlways :: States -> [AlwaysBlock] -> HS [AlwaysBlock]
m_flattenToAlways (st, ast) = flip (foldM (\as ir -> flattenIR st ir as)) (st^.irs)
  where
    flattenIR :: St -> IR -> [AlwaysBlock] -> HS [AlwaysBlock]
    flattenIR stt (Always{..}) l = do
      i <- get
      put (i+1)
      let stt' = filterSt alwaysStmt stt
          a    = AB { _aEvent   = event
                    , _aStmt    = alwaysStmt
                    , _aId      = i
                    , _aSt      = stt'
                    -- , _aAnnotSt = ast
                    , _aMd      = mempty
                    , _aLoc     = alwaysLoc
                    }
      return (a:l)
    flattenIR _  (ModuleInst{..}) l =
      m_flattenToAlways (modInstSt, ast) l

-- | Filter the ports and the uninterpreted functions of the state
filterSt :: Stmt -> St -> St
filterSt s st =
  over ports (filterVars vars') .
  set  ufs   ufs' .
  set  irs   [] $
  st
  where
    vars0 = HS.fromList $ foldVariables s -- all variables used in the statement
    ufs'  = filterMap vars0 (st^.ufs)
    vars' = vars0 `HS.union` (HS.fromList $ concat $ snd <$> HM.elems ufs')

-- | Keep the keys that are present in the given set
filterMap :: HS.HashSet Id -> UFMap -> UFMap
filterMap toKeep = HM.filterWithKey (\k _ -> HS.member k toKeep)

filterVars :: HS.HashSet Id -> [Var] -> [Var]
filterVars toKeep = filter (\v -> HS.member (varName v) toKeep)

