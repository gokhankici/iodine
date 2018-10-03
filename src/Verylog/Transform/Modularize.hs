{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (flatten) where

import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS

import           Verylog.Language.Types
import           Verylog.Transform.Utils

flatten :: St -> [AlwaysBlock]
flatten = flattenToAlways

-----------------------------------------------------------------------------------
-- | St -> [AlwaysBlock] :::: Flatten the module hierarchy
-----------------------------------------------------------------------------------

type HS = State Int

flattenToAlways :: St -> [AlwaysBlock]
flattenToAlways st = evalState (m_flattenToAlways st []) 0

m_flattenToAlways :: St -> [AlwaysBlock] -> HS [AlwaysBlock]
m_flattenToAlways st l = foldM (\as ir -> flattenIR st ir as) l (st^.irs)
  where
    flattenIR :: St -> IR -> [AlwaysBlock] -> HS [AlwaysBlock]
    flattenIR stt (Always{..}) l' = do
      i <- get
      put (i+1)
      return $ (AB event alwaysStmt i (filterSt alwaysStmt stt) alwaysLoc):l'
    flattenIR _  (ModuleInst{..}) l' =
      m_flattenToAlways modInstSt l'

    filterVars :: HS.HashSet Id -> [Var] -> [Var]
    filterVars toKeep = filter (\v -> HS.member (varName v) toKeep)

    filterList :: HS.HashSet Id -> [Id] -> [Id]
    filterList toKeep = filter (\x -> HS.member x toKeep)

    filterMap :: HS.HashSet Id -> UFMap -> UFMap
    filterMap toKeep = HM.filterWithKey (\k _ -> HS.member k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s stt = let vars  = HS.fromList $ foldVariables s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars `HS.union` (HS.fromList $ concat $ snd <$> HM.elems (st'^.ufs))
                         lhss  = getLhss s
                         st''  = over ports    (filterVars vars') .
                                 over sources  (filterList vars') .
                                 over assertEq (filterList vars') .
                                 over sinks    (filterList lhss) .
                                 over sanitize (filterList vars') $
                                 st'
                     in st''

