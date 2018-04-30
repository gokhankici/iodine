{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (flatten) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.IntMap.Strict         as IM
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as HS
import qualified Data.IntSet                as IS
import           Data.List
import           Data.Monoid

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

import           Verylog.Language.Types

-- import Debug.Trace

flatten :: St -> [AlwaysBlock]
flatten = flattenToAlways >>> removeWires

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

    filterMap :: HS.HashSet Id -> M.HashMap Id [Id] -> M.HashMap Id [Id]
    filterMap toKeep = M.filterWithKey (\k _v -> HS.member k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s stt = let vars  = HS.fromList $ foldVariables id s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars `HS.union` (HS.fromList $ concat $ M.elems (st'^.ufs))
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

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------

type WireMap = M.HashMap Id IS.IntSet
type EdgeMap = IM.IntMap IS.IntSet

removeWires :: [AlwaysBlock] -> [AlwaysBlock]
removeWires as = res -- trace (prettify g) res
  where
    res = mkNewAB <$> components g

    mkNewAB :: [Node] -> AlwaysBlock
    mkNewAB ns =
      let is     = topsort $ subgraph ns g
          blocks = (\i -> IM.findWithDefault (error "") i abMap) <$> is
          evnt   = getEvent blocks
          stmt   = Block [ a ^. aStmt | a <- blocks ]
          id'    = (head blocks) ^. aId + maxId
          st'    = foldl' (\s a -> s <> (a ^. aSt)) ((head blocks) ^. aSt) (tail blocks)
          loc    = (last blocks) ^. aLoc
      in  AB { _aEvent = evnt
             , _aStmt  = stmt
             , _aId    = id'
             , _aSt    = st'
             , _aLoc   = loc
             }

    getEvent :: [AlwaysBlock] -> Event
    getEvent []      = Star
    getEvent (a:as') = case a ^. aEvent of
                         PosEdge c -> PosEdge c
                         NegEdge c -> NegEdge c
                         Star      -> getEvent as'
      
    g :: UGr
    g  = makeGraph as $ wireUseEdges as

    abMap :: IM.IntMap AlwaysBlock
    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as

    maxId :: Int
    maxId = fst $ IM.findMax abMap


makeGraph :: [AlwaysBlock] -> EdgeMap -> UGr
makeGraph as es = 
  if   all ((< 2) . length) (scc g)
  then g
  else error $
       "graph g contains a cycle:\n" ++
       prettify g ++ "\n\n" ++
       intercalate "\n" (show <$> as)

  where
    g = mkGraph
        ((\i -> (i,())) <$> IM.keys es)
        [ (frNode, toNode, ())
        | (frNode, toSet) <- IM.assocs es
        , toNode <- IS.toList toSet
        ]

wireUseEdges :: [AlwaysBlock] -> EdgeMap
wireUseEdges as =
  foldl' (\m a -> if   (a ^. aId) `IM.member` edges1
                  then m
                  else if   case find varIsReg (a ^. aSt ^. ports) of
                              Just _  -> True
                              Nothing -> False
                       then IM.insert (a ^. aId) IS.empty m
                       else m
         ) edges1 as
  where

    varIsReg :: Var -> Bool
    varIsReg (Wire _)     = False
    varIsReg (Register _) = True

    edges1 :: EdgeMap
    edges1 = M.foldlWithKey' helper IM.empty wireWriteMap

    helper :: EdgeMap -> Id -> IS.IntSet -> EdgeMap
    helper es w ws = 
      case M.lookup w wireReadMap of
        Nothing -> es
        Just rs -> helper2 ws rs es

    helper2 :: IS.IntSet -> IS.IntSet -> EdgeMap -> EdgeMap
    helper2 ws rs es = 
      IS.foldl' (\es' wi -> IM.alter (\m -> case m of
                                              Nothing  -> Just rs
                                              Just rs' -> Just $ rs' `IS.union` rs
                                     ) wi es'
                ) es ws

    wireReadMap, wireWriteMap :: WireMap
    (wireReadMap, wireWriteMap) = foldl' insertWritesToWires (M.empty, M.empty) as


insertWritesToWires :: (WireMap, WireMap) -> AlwaysBlock -> (WireMap, WireMap)
insertWritesToWires (readMap, writeMap) a =
  ( updateMap readMap readSet
  , updateMap writeMap writeSet
  )
  where
    i :: Int
    i = a ^. aId

    updateMap :: WireMap -> HS.HashSet Id -> WireMap
    updateMap m s = HS.foldl' addToSet m s

    (readSet, writeSet) = wireUpdates (a ^. aStmt)

    addToSet :: WireMap -> Id -> WireMap
    addToSet m' w = let r = M.alter alterF w m'
                    in r `seq` r

    alterF (Nothing) = Just $ IS.singleton i
    alterF (Just s)  = Just $ IS.insert i s

    -- first element is read set, second is write set
    wireUpdates :: Stmt -> (HS.HashSet Id, HS.HashSet Id)
    wireUpdates (Block ss)            = let r = mconcat (wireUpdates <$> ss) in seq r r
    wireUpdates (BlockingAsgn l r)    = (checkIfWire r, checkIfWire l)
    wireUpdates (NonBlockingAsgn l r) = (checkIfWire r, checkIfWire l)
    wireUpdates (IfStmt c th el)      = let r = wireUpdates th <> wireUpdates el <> (checkIfWire c, HS.empty)
                                        in seq r r
    wireUpdates Skip                  = (HS.empty, HS.empty)

    checkIfWire :: Id -> HS.HashSet Id
    checkIfWire v = if   (Wire v) `elem` (a ^. aSt ^. ports)
                    then HS.singleton v
                    else case  v `M.lookup` (a ^. aSt ^. ufs) of
                           Nothing -> HS.empty
                           Just vs -> mconcat (checkIfWire <$> vs)
                               

