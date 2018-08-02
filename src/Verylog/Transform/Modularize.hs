{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Modularize (flatten) where

import           Control.Arrow
import           Control.Lens hiding (mapping)
import           Control.Monad.State.Lazy
import qualified Data.IntMap.Strict         as IM
import qualified Data.HashMap.Strict        as HM
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
-- import Debug.Trace

flatten :: St -> [AlwaysBlock]
flatten = flattenToAlways >>>
          mergeStars      >>>
          mergeClocks     >>>
          removeWires

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

    filterMap :: HS.HashSet Id -> HM.HashMap Id [Id] -> HM.HashMap Id [Id]
    filterMap toKeep = HM.filterWithKey (\k _v -> HS.member k toKeep)

    filterSt :: Stmt -> St -> St
    filterSt s stt = let vars  = HS.fromList $ foldVariables s
                         st'   = over ufs      (filterMap vars)  .
                                 set irs      [] $
                                 stt
                         vars' = vars `HS.union` (HS.fromList $ concat $ HM.elems (st'^.ufs))
                         lhss  = getLhss s
                         st''  = over ports    (filterVars vars') .
                                 over sources  (filterList vars') .
                                 -- over sinks    (filterList vars') .
                                 over sinks    (filterList lhss) .
                                 over sanitize (filterList vars') $
                                 st'
                     in st''

getLhss :: Stmt -> S
getLhss s = h s
  where
    h Skip                  = HS.empty
    h (BlockingAsgn{..})    = HS.singleton lhs
    h (NonBlockingAsgn{..}) = HS.singleton lhs
    h (IfStmt{..})          = foldMap h [thenStmt, elseStmt]
    h (Block{..})           = foldMap h blockStmts

getRhss :: HM.HashMap Id [Id] -> Stmt -> S
getRhss us s = h s
  where
    h Skip                  = HS.empty
    h (BlockingAsgn{..})    = lukap rhs
    h (NonBlockingAsgn{..}) = lukap rhs
    h (IfStmt{..})          = lukap ifCond <> foldMap h [thenStmt, elseStmt]
    h (Block{..})           = foldMap h blockStmts

    lukap :: Id -> S
    lukap v = case HM.lookup v us of
                Nothing -> HS.singleton v
                Just vs -> HS.fromList vs


------------------------------------------------------------------------------------
-- | mergeStars :: [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks with @(*)
------------------------------------------------------------------------------------
type S  = HS.HashSet Id
type M  = HM.HashMap Int S
type M2 = HM.HashMap Id IS.IntSet

mergeStars :: [AlwaysBlock] -> [AlwaysBlock]
mergeStars as = stars' ++ others
  where
    (stars, others) = foldl' (\(ss,os) a -> if   a ^. aEvent == Star
                                            then (a:ss,os)
                                            else (ss, a:os)) ([],[]) as

    stars' = if   hasCycle g
             then error "stars' has a cycle"
             else trc "merges:" merges merges
                  
    merges :: [AlwaysBlock]
    merges = [ let g'  = subgraph c g
                   ns  = topsort g'
                   as' = (abMap IM.!) <$> ns
               in  AB { _aEvent = Star
                      , _aStmt  = Block $ view aStmt <$> as'
                      , _aId    = n + maxId
                      , _aSt    = mconcat $ view aSt <$> as'
                      , _aLoc   = ("* join", "* join")
                      }
             | (n, c) <- zip [1..] (components g)
             ]

    g :: Gr () ()
    g = mkGraph (((\n -> (n,())) . view aId) <$> stars) es

    es :: [(Int, Int, ())]
    es =
      HM.foldlWithKey'
      (\l n s ->
          -- ns : all blocks that update the sensitivity list of block# n
          let ns = IS.toList $ foldMap (\v -> HM.lookupDefault IS.empty v sensitizers) s
          -- (n1, n2) means n1's block is before after n2 executes
          in ((\n' -> (n',n,())) <$> ns) ++ l
      )
      []
      sensitivitySets

    -- block # ==> sensitivity list
    sensitivitySets :: M
    sensitivitySets = HM.fromList $
                      (\a -> (a ^. aId, getRhss (a ^. aSt ^. ufs) (a ^. aStmt))) <$> stars

    -- v: variable ==> {n:block k# | n updates v}
    sensitizers :: M2
    sensitizers =
      let f v Nothing  = Just $ IS.singleton v
          f v (Just s) = Just $ IS.insert v s
      in HM.foldlWithKey'
         (\m n s -> HS.foldl' (\m' v -> HM.alter (f n) v m') m s)
         HM.empty
         writeSets

    -- block # ==> update list
    writeSets :: M
    writeSets = HM.fromList $ 
                      (\a -> (a ^. aId, getLhss (a ^. aStmt))) <$> stars

    abMap :: IM.IntMap AlwaysBlock
    abMap = IM.fromList $ (\a -> (a^.aId, a)) <$> as

    maxId :: Int
    maxId = fst $ IM.findMax abMap
  
-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------

mergeClocks :: [AlwaysBlock] -> [AlwaysBlock]
mergeClocks as = mergeGroup posAs 1 ++
                 mergeGroup negAs 2 ++
                 rest
  where
    (posAs, negAs, rest) =
      foldl' (\(ps,ns,rs) a -> case a ^. aEvent of
                                 PosEdge _ -> (a:ps, ns,   rs)
                                 NegEdge _ -> (ps,   a:ns, rs)
                                 _         -> (ps,   ns,   a:rs)) ([],[],[]) as

    maxId = maximum $ (view aId) <$> as

    mergeGroup gs n =
      let (gs', rs) = partition (allNBs . view aStmt) gs

          allNBs Skip                  = True
          allNBs (BlockingAsgn{..})    = False
          allNBs (NonBlockingAsgn{..}) = True
          allNBs (IfStmt{..})          = all allNBs [thenStmt, elseStmt]
          allNBs (Block{..})           = all allNBs blockStmts

          a' = AB { _aEvent = head gs' ^. aEvent
                  , _aStmt  = Block $ view aStmt <$> gs'
                  , _aId    = n + maxId
                  , _aSt    = mconcat $ view aSt <$> gs'
                  , _aLoc   = ("clk join", "clk join")
                  }
      in case gs' of
           [] -> gs
           _  -> a' : rs

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------

type WireMap = HM.HashMap Id IS.IntSet
type EdgeMap = IM.IntMap IS.IntSet

type G = Gr Int ()

removeWires :: [AlwaysBlock] -> [AlwaysBlock]
removeWires as_pre = dbg
                 ( printf "individual blocks:\n%s\n\n#blocks after removing wires: %d -> %d"
                   as_str (length as) (length res)
                 )
                 res
  where
    as = as_pre
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
    eventCheck xs = if   all (not . isClk . view aEvent) (init xs)
                    then xs
                    else error $
                         "all blocks except the last one should be always@(*)\n" ++
                         show xs
      
    -- block # -> block
    abMap :: IM.IntMap AlwaysBlock
    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as

    -- max id of the always blocks
    maxId :: Int
    maxId    = fst $ IM.findMax abMap

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
      let allUpds  = HM.foldlWithKey' (\iss w is -> if   (IS.findMin is) `elem` ns
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
  if   hasCycle g
  then throw $
       CycleError { cycleStr      = myPrintG (nfilter (\n -> n `elem` cyc) g)
                  , cycleErrorStr = intercalate "\n" (f <$> ns) 
                  }
  else g
  where
    f n   = let a = abmap IM.! n
            in  show $ a^.aStmt
    abmap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    cs    = scc g
    cyc   = head $ filter (\l -> length l > 1) cs -- pick the first cycle
    g'    = subgraph cyc g
    ns    = reverse $ topsort g'

hasCycle :: Gr a b -> Bool
hasCycle g = any ((>= 2) . length) (scc g)

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
    dupWriteMap = HM.filter (\s -> IS.size s > 1) wireWriteMap

    edges1 :: EdgeMap
    edges1 = HM.foldlWithKey' helper IM.empty wireWriteMap

    helper :: EdgeMap -> Id -> IS.IntSet -> EdgeMap
    helper es w ws = 
      case HM.lookup w wireReadMap of
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
    (wireReadMap, wireWriteMap) = foldl' insertWritesToVars (HM.empty, HM.empty) as


insertWritesToVars :: (WireMap, WireMap) -> AlwaysBlock -> (WireMap, WireMap)
insertWritesToVars (readMap, writeMap) a =
  ( updateMap readMap readSet
  , case a ^. aEvent of
      Assign -> updateMap writeMap writeSet -- only merge `assign` blocks
      _      -> writeMap
  )
  where
    i :: Int
    i = a ^. aId

    updateMap :: WireMap -> HS.HashSet Id -> WireMap
    updateMap m s = HS.foldl' addToSet m s

    (readSet, writeSet) = varUpdates (a ^. aStmt)

    addToSet :: WireMap -> Id -> WireMap
    addToSet m' w = HM.alter alterF w m'

    alterF (Nothing) = Just $ IS.singleton i
    alterF (Just s)  = Just $ IS.insert i s

    -- first element is read set, second is write set
    varUpdates :: Stmt -> (HS.HashSet Id, HS.HashSet Id)
    varUpdates (Block ss)            = let r = mconcat (varUpdates <$> ss) in seq r r
    varUpdates s@(BlockingAsgn l r)    =
      let res   = (findUsedWires r, findUsedWires l)
          event = a ^. aEvent
      in if   isWire l
         then if   not $ isClk event
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

    findUsedWires :: Id -> HS.HashSet Id
    findUsedWires v | isWire v  = HS.singleton v
                    | otherwise = case  v `HM.lookup` (a ^. aSt ^. ufs) of
                                    Nothing -> HS.empty
                                    Just vs -> mconcat (findUsedWires <$> vs)

myPrintG   :: G -> String
myPrintG g = showDot $ fglToDotGeneric g show (const "") id
