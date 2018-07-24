{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (flatten) where

import           Control.Arrow
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
import Data.Graph.Inductive.Query hiding (trc)

import           Verylog.Language.Types
import           Verylog.Transform.Utils

import Control.Exception

import Text.Printf
import Data.Graph.Inductive.Dot

flatten :: St -> [AlwaysBlock]
flatten = flattenToAlways >>> removeWires

-----------------------------------------------------------------------------------
-- | St -> [AlwaysBlock] :::: Flatten the module hierarchy
-----------------------------------------------------------------------------------

type HS = State Int
type S  = HS.HashSet Id

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
    filterSt s stt = let vars  = HS.fromList $ foldVariables s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars `HS.union` (HS.fromList $ concat $ M.elems (st'^.ufs))
                         lhss  = HS.fromList $ getLhss s
                         st''  = over ports    (filterVars vars') .
                                 over sources  (filterList vars') .
                                 -- over sinks    (filterList vars') .
                                 over sinks    (filterList lhss) .
                                 over sanitize (filterList vars') $
                                 st'
                     in st''

    
    getLhss Skip                  = []
    getLhss (BlockingAsgn{..})    = [lhs]
    getLhss (NonBlockingAsgn{..}) = [lhs]
    getLhss (IfStmt{..})          = getLhss thenStmt ++ getLhss elseStmt
    getLhss (Block{..})           = concatMap getLhss blockStmts


-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------

type WireMap = M.HashMap Id IS.IntSet
type EdgeMap = IM.IntMap IS.IntSet

type G = Gr Int ()

removeWires :: [AlwaysBlock] -> [AlwaysBlock]
removeWires as = dbg
                 ( printf "individual blocks:\n%s\n\n#blocks after removing wires: %d -> %d"
                   as_str (length as) (length res)
                 )
                 res
  where
    as_str = intercalate "\n\n" (show <$> as)
    res = snd $
          foldl'
          (\(n,l) ns -> ( n+1
                        , (mkNewAB n ns):l
                        ))
          (maxId + 1,[])
          (calcSubgraphs dupWriteMap globalG)

    mkNewAB :: Int -> G -> AlwaysBlock
    mkNewAB id' gr =
      let is     = topsort $ checkCycles as gr
          blocks = eventCheck $ (\i -> IM.findWithDefault (error "") i abMap) <$> is
          evnt   = getEvent blocks
          stmt   = Block [ a ^. aStmt | a <- blocks ]
          st'    = foldl' (\s a -> s <> (a ^. aSt)) ((head blocks) ^. aSt) (tail blocks)
          loc    = (last blocks) ^. aLoc
          ab     = AB { _aEvent   = evnt
                      , _aStmt    = stmt
                      , _aId      = id'
                      , _aSt      = st'
                      , _aLoc     = loc
                      }
      in dbg (printf "combined %s into %d" (show $ fst <$> labNodes gr) id') ab

    getEvent :: [AlwaysBlock] -> Event
    getEvent xs = (last xs) ^. aEvent

    -- make global assignment graph from edge map
    globalG :: G
    globalG = let _g = makeGraph es
              in  dbg (myPrintG _g) _g

    es                :: EdgeMap
    dupWriteMap       :: WireMap
    (es, dupWriteMap) = wireUseEdges as

    eventCheck :: [AlwaysBlock] -> [AlwaysBlock]
    eventCheck xs = if   all (\a -> a^.aEvent == Star) (init xs)
                    then xs
                    else error $
                         "all blocks except the last one should be always@(*)\n" ++
                         show xs
      
    -- block # -> block
    abMap :: IM.IntMap AlwaysBlock
    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as

    -- max id of the always blocks
    maxId :: Int
    maxId = fst $ IM.findMax abMap

calcSubgraphs :: WireMap -> G -> [G]
calcSubgraphs dupWriteMap g = concat [ pathsToLeaves (subgraph ns g) | ns <- combinedNodes ]
  where
    pathsToLeaves :: G -> [G]
    pathsToLeaves gr = [ parentG r | r <- roots ]
      where
        rootG     = gfiltermap (\c -> if suc' c == [] then Just c else Nothing) gr
        roots     = fst <$> labNodes rootG
        parentG r = subgraph (rdfs [r] gr) gr

    combinedNodes :: [[Node]]
    combinedNodes = concat [ generateNodes ns | ns <- components g ]

    generateNodes :: [Node] -> [[Node]]
    generateNodes ns =
      let allUpds  = M.foldlWithKey' (\iss w is -> if   (IS.findMin is) `elem` ns
                                                   then (w, IS.toList is):iss
                                                   else iss
                                     ) [] dupWriteMap
          nsToDrop = map concat $ mapM allDropOnes $ snd <$> allUpds -- seems like it's doing the right thing ...
      in if   allUpds == []
         then [ns]
         else let res = [ ns \\ nsNeg | nsNeg <- nsToDrop ]
              in  trc "duplicate updates: " allUpds res

    allDropOnes :: [a] -> [[a]]
    allDropOnes as = helper (as, [])
      where
        helper ([], _)    = []
        helper (a:as', l) =
          let r = as' ++ l in (seq r r) : (helper (as', a:l))

makeGraph :: EdgeMap -> G
makeGraph es =
  mkGraph
  ((\i -> (i,i)) <$> IM.keys es)
  [ (frNode, toNode, ())
  | (frNode, toSet) <- IM.assocs es
  , toNode <- IS.toList toSet
  , frNode /= toNode
  ]

checkCycles :: [AlwaysBlock] -> G -> G
checkCycles as g =
  if   any ((>= 2) . length) (scc g)
  then throw $
       CycleError { cycleStr      = myPrintG (nfilter (\n -> n `elem` dups) g)
                  , cycleErrorStr = intercalate "\n\n"
                                    (reverse $ show <$> (filter (\a -> (a ^. aId) `elem` dups) as))
                  }
  else g
  where
    cs   = scc g
    dups = head $ filter (\l -> length l > 1) cs -- pick the first cycle

wireUseEdges :: [AlwaysBlock] -> (EdgeMap, WireMap)
wireUseEdges as = (edgeMap, dupWriteMap)
  where
    edgeMap =
      foldl' (\m a -> if   (a ^. aId) `IM.member` edges1
                      then m
                      else if   case find isRegister (a ^. aSt ^. ports) of
                                  Just _  -> True
                                  Nothing -> False
                           then IM.insert (a ^. aId) IS.empty m
                           else m
             ) edges1 as

    dupWriteMap :: WireMap
    dupWriteMap = M.filter (\s -> IS.size s > 1) wireWriteMap

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


    -- -- all the registers that are ok to merge with
    -- _okRegs = HS.unions $ (\a -> case a ^. aEvent of
    --                               Star -> h (a^.aStmt)
    --                               _    -> HS.empty
    --                      ) <$> as

    -- h ::  Stmt -> S
    -- h Skip                  = HS.empty
    -- h (BlockingAsgn{..})    = HS.singleton lhs
    -- h (NonBlockingAsgn{..}) = HS.empty
    -- h (IfStmt{..})          = HS.union (h thenStmt) (h elseStmt)
    -- h (Block{..})           = HS.unions (h <$> blockStmts)

    -- okRegs = _okRegs `deepseq` _okRegs

    wireReadMap, wireWriteMap :: WireMap
    (wireReadMap, wireWriteMap) = foldl' insertWritesToVars (M.empty, M.empty) as


insertWritesToVars :: (WireMap, WireMap) -> AlwaysBlock -> (WireMap, WireMap)
insertWritesToVars (readMap, writeMap) a =
  ( updateMap readMap readSet
  , updateMap writeMap writeSet
  )
  where
    i :: Int
    i = a ^. aId

    updateMap :: WireMap -> HS.HashSet Id -> WireMap
    updateMap m s = HS.foldl' addToSet m s

    (readSet, writeSet) = varUpdates (a ^. aStmt)

    addToSet :: WireMap -> Id -> WireMap
    addToSet m' w = M.alter alterF w m'

    alterF (Nothing) = Just $ IS.singleton i
    alterF (Just s)  = Just $ IS.insert i s

    -- first element is read set, second is write set
    varUpdates :: Stmt -> (HS.HashSet Id, HS.HashSet Id)
    varUpdates (Block ss)            = let r = mconcat (varUpdates <$> ss) in seq r r
    varUpdates s@(BlockingAsgn l r)    =
      let res = (findUsedWires r, findUsedWires l)
      in if   isWire l
         then if   (a^.aEvent) == Star
              then res
              else error $
                   "assignment to wire in non-star blocking assignment" -- sanity check
                   ++ "\n" ++ show s
         else res -- error $ "blocking assignment to non-wire: " ++ l
    varUpdates s@(NonBlockingAsgn l r) =
      let res = (findUsedWires r, findUsedWires l)
      in if   isWire l
         then error $
              "non-blocking assignment to a wire" -- sanity check
              ++ "\n" ++ show s
         else res
    varUpdates (IfStmt c th el) = let r = (findUsedWires c, HS.empty) <> varUpdates th <> varUpdates el
                                   in seq r r
    varUpdates Skip             = (HS.empty, HS.empty)

    isWire v = (Wire v) `elem` (a ^. aSt ^. ports)

    -- isOKReg v = HS.member v okRegs

    findUsedWires :: Id -> HS.HashSet Id
    findUsedWires v | isWire v  = HS.singleton v
                    --- | isOKReg v = HS.singleton v
                    | otherwise = case  v `M.lookup` (a ^. aSt ^. ufs) of
                                    Nothing -> HS.empty
                                    Just vs -> mconcat (findUsedWires <$> vs)

myPrintG   :: G -> String
myPrintG g = showDot $ fglToDotGeneric g show (const "") id
