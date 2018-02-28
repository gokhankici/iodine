{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (modularize) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.List                  as Li
import qualified Data.HashMap.Strict        as M

import           Verylog.Language.Types

modularize = inlineVariables >>> flattenToAlways

-----------------------------------------------------------------------------------
-- | St -> St :::: inline module instantiation arguments
-----------------------------------------------------------------------------------

type Args = M.HashMap Id Id

inlineVariables :: St -> St
inlineVariables st = st & irs .~ map (mapIR M.empty) (st ^. irs)
  where
    mapIR :: Args -> IR -> IR
    mapIR args (Always{..})        = Always e' (inline args alwaysStmt)
      where
        e' = case event of
               Star      -> Star
               PosEdge v -> PosEdge $ replaceIfFound args v
               NegEdge v -> NegEdge $ replaceIfFound args v

    mapIR args ir@(ModuleInst{..}) = ir { modInstSt = st' }
      where args' = M.union (M.fromList modInstArgs) args
            st'   = evalState (comp args') modInstSt

    comp :: Args -> State St St
    comp args = do irs      %= map (mapIR args)
                   ports    %= replaceVars args
                   ufs      %= replaceUFArgs args
                   sanitize %= replaceVars args
                   get

    inline :: Args -> Stmt -> Stmt
    inline args s = case s of
                      Block ss            -> Block (ia <$> ss)
                      BlockingAsgn l r    -> BlockingAsgn (rif l) (rif r)
                      NonBlockingAsgn l r -> NonBlockingAsgn (rif l) (rif r)
                      IfStmt c t e        -> IfStmt (rif c) (ia t) (ia e)
                      Skip                -> Skip
      where
        rif = replaceIfFound args
        ia  = inline args

    replaceVars :: Args -> [Id] -> [Id]
    replaceVars args = map (replaceIfFound args)

    replaceUFArgs          :: Args -> M.HashMap Id [Id] -> M.HashMap Id [Id]
    replaceUFArgs args ufs = M.map (replaceVars args) ufs

    replaceIfFound        :: Args -> Id -> Id
    replaceIfFound args v = M.lookupDefault v v args

-----------------------------------------------------------------------------------
-- | St -> [AlwaysBlock] :::: Flatten the module hierarchy after inlining
-----------------------------------------------------------------------------------

type S = State Int

flattenToAlways :: St -> [AlwaysBlock]
flattenToAlways st = evalState (m_flattenToAlways st) 0

m_flattenToAlways :: St -> S [AlwaysBlock]
m_flattenToAlways st = sequence ((f st) <$> st^.irs) >>= return . concat
  where
    f                     :: St -> IR -> S [AlwaysBlock]
    f st (Always{..})     =  do id <- get
                                put (id+1)
                                return [AB event alwaysStmt id (filterSt alwaysStmt st)]
    f _  (ModuleInst{..}) =  m_flattenToAlways modInstSt

    filterList :: [Id] -> [Id] -> [Id]
    filterList toKeep = filter (\x -> Li.elem x toKeep)

    filterMap :: [Id] -> M.HashMap Id [Id] -> M.HashMap Id [Id]
    filterMap toKeep = M.filterWithKey (\k _v -> Li.elem k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s st = let vars  = foldVariables id s
                        st'   = over ufs      (filterMap vars)  .
                                set irs      [] $
                                st
                        vars' = vars ++ (concat $ M.elems (st'^.ufs))
                        st''  = over ports    (filterList vars') .
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
  foldVariables f (Always _ s) = foldVariables f s
  foldVariables _ _            = throw (PassError "foldVariables called on non-always block")
