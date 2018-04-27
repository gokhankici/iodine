{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (flatten) where

import           Control.Exception
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S

import           Verylog.Language.Types

flatten :: St -> [AlwaysBlock]
flatten = flattenToAlways

-----------------------------------------------------------------------------------
-- | St -> [AlwaysBlock] :::: Flatten the module hierarchy
-----------------------------------------------------------------------------------

type S = State Int

flattenToAlways :: St -> [AlwaysBlock]
flattenToAlways st = evalState (m_flattenToAlways st []) 0

m_flattenToAlways :: St -> [AlwaysBlock] -> S [AlwaysBlock]
m_flattenToAlways st l = foldM (\as ir -> flattenIR st ir as) l (st^.irs)
  where
    flattenIR :: St -> IR -> [AlwaysBlock] -> S [AlwaysBlock]
    flattenIR stt (Always{..}) l' = do
      i <- get
      put (i+1)
      return $ (AB event alwaysStmt i (filterSt alwaysStmt stt) alwaysLoc):l'
    flattenIR _  (ModuleInst{..}) l' =
      m_flattenToAlways modInstSt l'

    filterVars :: S.HashSet Id -> [Var] -> [Var]
    filterVars toKeep = filter (\v -> S.member (varName v) toKeep)

    filterList :: S.HashSet Id -> [Id] -> [Id]
    filterList toKeep = filter (\x -> S.member x toKeep)

    filterMap :: S.HashSet Id -> M.HashMap Id [Id] -> M.HashMap Id [Id]
    filterMap toKeep = M.filterWithKey (\k _v -> S.member k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s stt = let vars  = S.fromList $ foldVariables id s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars `S.union` (S.fromList $ concat $ M.elems (st'^.ufs))
                         st''  = over ports    (filterVars vars') .
                                 over sources  (filterList vars') .
                                 over sinks    (filterList vars') .
                                 over sanitize (filterList vars') $
                                 st'
                     in st''

class FoldVariables a where
  foldVariables :: (Id -> b) -> a -> [b]

instance FoldVariables Stmt where
  foldVariables f (Block ss)            = concatMap (foldVariables f) ss
  foldVariables f (BlockingAsgn l r)    = [f l, f r]
  foldVariables f (NonBlockingAsgn l r) = [f l, f r]
  foldVariables f (IfStmt c t e)        = [f c] ++ concatMap (foldVariables f) [t,e]
  foldVariables _ Skip                  = []

instance FoldVariables IR where
  foldVariables f (Always _ s _) = foldVariables f s
  foldVariables _ _              = throw (PassError "foldVariables called on non-always block")
