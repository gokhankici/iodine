{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (modularize) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.List                  as Li
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S

import           Verylog.Language.Types
import           Debug.Trace

modularize :: St -> [AlwaysBlock]
modularize = inlineVariables >>> flattenToAlways

-----------------------------------------------------------------------------------
-- | St -> St :::: inline module instantiation arguments
-----------------------------------------------------------------------------------

type CurrentVars     = [Var]  
type ParentChildVars = ([Var], [Var])
type NameMapping     = M.HashMap Id Id

--------------------------------------------------------------------------------
inlineVariables :: St -> St
--------------------------------------------------------------------------------
inlineVariables st' =
  st & irs .~ map (renameIR M.empty (st ^. ports)) (st ^. irs)
  where
    st = st' -- trace (show st') st'

--------------------------------------------------------------------------------
renameIR :: NameMapping -> CurrentVars -> IR -> IR
-- takes a mapping & variables of the current module,
-- and renames the variables in the IR
--------------------------------------------------------------------------------
renameIR mapping _ (Always{..}) = Always e' (inlineStmt mapping alwaysStmt) alwaysLoc
  where
    e' = case event of
           Star      -> Star
           PosEdge v -> PosEdge $ replaceId mapping v
           NegEdge v -> NegEdge $ replaceId mapping v

renameIR mapping parentVars ir@(ModuleInst{..}) =
  ir { modInstSt   = st2
     , modInstArgs = modInstArgs'
     }
  where
    childVars = modInstSt ^. ports

    -- replace the actual parameters first, since they might have changed
    modInstArgs'           = (\a -> a & _2 %~ replaceId mapping) <$> modInstArgs
    (mapping', newAssigns) = createMapping mapping (parentVars, childVars) modInstArgs'

    st0 = over ports (parentVars ++) modInstSt
    st1 = inlineModInst mapping' st0
    st2 = st1 & irs %~ (++) newAssigns

--------------------------------------------------------------------------------
createMapping :: NameMapping -> ParentChildVars -> [(Port, Id)] -> (NameMapping, [IR])
-- takes a current mapping, both parent and child variables, and the module instantiation arguments
-- then updates the mapping and creates `assign` blocks if needed for the input/output ports if needed
--------------------------------------------------------------------------------
createMapping mapping (parentVars, childVars) args = foldr (\(p,a) m -> helper p a m) (mapping, []) args
  where
    mkAsgn asgnFrom asgnTo =
      Always { event      = Star
             , alwaysStmt = BlockingAsgn { lhs = asgnTo
                                         , rhs = asgnFrom
                                         }
             , alwaysLoc  = ("auto generated", "auto generated")
             }
    helper p a (m,l) = 
      case (findVar parentVars a, findVar childVars (portName p)) of
        (Just (Wire parentVar)     , Just (Wire childVar))     -> (M.insert childVar parentVar m, l)
        (Just (Register parentVar) , Just (Register childVar)) ->
          throw (PassError $ parentVar ++ " -> " ++ childVar ++ " and both are registers")
        _ ->
          case p of
            Input _   -> (m, (mkAsgn a (portName p)) : l) -- actual => formal
            Output _  -> (m, (mkAsgn (portName p) a) : l) -- formal => actual

    findVar vars name = Li.find ((== name) . varName) vars


--------------------------------------------------------------------------------
inlineStmt :: NameMapping -> Stmt -> Stmt
--------------------------------------------------------------------------------
inlineStmt mapping s =
  case s of
    Block ss            -> Block (ia <$> ss)
    BlockingAsgn l r    -> BlockingAsgn (rif l) (rif r)
    NonBlockingAsgn l r -> NonBlockingAsgn (rif l) (rif r)
    IfStmt c t e        -> IfStmt (rif c) (ia t) (ia e)
    Skip                -> Skip
  where
    rif = replaceId mapping
    ia  = inlineStmt mapping

inlineModInst :: NameMapping -> St -> St
inlineModInst mapping st =
  over irs          (map (renameIR mapping newPorts)) .
  over sanitizeGlob rIdsMSet .
  over sanitize     rIdsMSet .
  over taintEq      rIdsMSet .
  over sinks        rIdsM .
  over sources      rIdsM .
  over ufs          (replaceUFArgs mapping) .
  set  ports        newPorts
  $ st
  where
    newPorts = (replacePorts mapping) <$> st ^. ports
    rIdsM    = replaceIds mapping
    rIdsMSet = S.toList . S.fromList . (replaceIds mapping)

replaceIds  :: NameMapping -> [Id] -> [Id]
replaceIds mapping = map (replaceId mapping)

replaceUFArgs :: NameMapping -> M.HashMap Id [Id] -> M.HashMap Id [Id]
replaceUFArgs mapping uninFs = M.map (replaceIds mapping) uninFs

replacePorts :: NameMapping -> Var -> Var 
replacePorts mapping v = v { varName = replaceId mapping (varName v) }

replaceId :: NameMapping -> Id -> Id
replaceId mapping v = M.lookupDefault v v mapping

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
