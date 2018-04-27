{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (modularize) where

import           Control.Exception
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.List                  as Li
import qualified Data.HashMap.Strict        as M

import           Verylog.Language.Types

modularize :: St -> [AlwaysBlock]
modularize = flattenToAlways

-----------------------------------------------------------------------------------
-- | St -> [AlwaysBlock] :::: Flatten the module hierarchy
-----------------------------------------------------------------------------------

type S = State Int

flattenToAlways :: St -> [AlwaysBlock]
flattenToAlways st = evalState (m_flattenToAlways st) 0

m_flattenToAlways :: St -> S [AlwaysBlock]
m_flattenToAlways st = sequence (f st <$> st^.irs) >>= return . concat
  where
    f                     :: St -> IR -> S [AlwaysBlock]
    f stt (Always{..})    =  do i <- get
                                put (i+1)
                                return [AB event alwaysStmt i (filterSt alwaysStmt stt) alwaysLoc]
    f _  (ModuleInst{..}) =  m_flattenToAlways modInstSt

    filterVars :: [Id] -> [Var] -> [Var]
    filterVars toKeep = filter (\v -> Li.elem (varName v) toKeep)


    filterList :: [Id] -> [Id] -> [Id]
    filterList toKeep = filter (\x -> Li.elem x toKeep)

    filterMap :: [Id] -> M.HashMap Id [Id] -> M.HashMap Id [Id]
    filterMap toKeep = M.filterWithKey (\k _v -> Li.elem k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s stt = let vars  = foldVariables id s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars ++ (concat $ M.elems (st'^.ufs))
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
