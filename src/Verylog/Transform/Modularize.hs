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

-- import Text.Printf
import Debug.Trace

flatten :: St -> [AlwaysBlock]
flatten st =
  let res = flattenToAlways >>>
            mergeStars >>>
            mergeClocks >>>
            removeWires $ st
      s   = intercalate "\n\n" $ show . view aStmt <$> res
  in  res -- trace (printf "as(#%d):\n%s" (length res) s) res

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
type M  = IM.IntMap S
type M2 = HM.HashMap Id IS.IntSet

mergeStars :: [AlwaysBlock] -> [AlwaysBlock]
mergeStars as = stars' ++ others
  where
    (stars, others) = foldl' (\(ss,os) a -> if   a ^. aEvent == Star
                                            then (a:ss,os)
                                            else (ss, a:os)) ([],[]) as

    stars' = if   hasCycle g
             then error "stars' has a cycle"
             else trace ("merge stars:\n" ++ intercalate "\n\n" (show . view aStmt <$> merges)) merges
                  
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

    -- rs: block # ==> sensitivity list
    -- ws: block # ==> update list
    rs = readSets  stars
    ws = writeSets stars

    g :: Gr () ()
    g = makeGraphFromRWSet rs ws

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

          a = AB { _aEvent = head gs' ^. aEvent
                 , _aStmt  = Block $ view aStmt <$> gs'
                 , _aId    = n + maxId
                 , _aSt    = mconcat $ view aSt <$> gs'
                 , _aLoc   = ("clk join", "clk join")
                 }
          a' = a -- trace (printf "merged clocks:\n%s" (show $ a ^. aStmt)) a
          
      in case gs' of
           [] -> gs
           _  -> a' : rs

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------

type G = Gr () ()

removeWires :: [AlwaysBlock] -> [AlwaysBlock]
removeWires as = [ mkAB (n+maxId) g' | (n,g') <- zip [1..] (invertedTrees globalG) ]
  where
    assigns = filter ((== Assign).(view aEvent)) as

    aMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    maxId = fst $ IM.findMax aMap
    
    readsMap  = readSets as
    writesMap = writeSets assigns

    globalG = makeGraphFromRWSet readsMap writesMap
  
    mkAB n g = 
      let is       = topsort g
          mergedAs = (aMap IM.!) <$> is
          lastA    = last mergedAs
      in AB { _aEvent = lastA ^. aEvent
            , _aStmt  = Block $ view aStmt <$> mergedAs
            , _aId    = n
            , _aSt    = mconcat $ view aSt <$> mergedAs
            , _aLoc   = ("clk join", "clk join")
            }

    invertedTrees :: G -> [G]
    invertedTrees gr = [ parentG r | r <- roots, isNonAssign r ]
      where
        rootG         = gfiltermap (\c -> if suc' c == [] then Just c else Nothing) gr
        roots         = fst <$> labNodes rootG
        parentG r     = subgraph (rdfs [r] gr) gr
        isNonAssign n = (aMap IM.! n) ^. aEvent /= Assign

hasCycle :: Gr a b -> Bool
hasCycle g = any ((>= 2) . length) (scc g)

readSets :: [AlwaysBlock] -> M
readSets as = IM.fromList $ (\a -> (a ^. aId, getRhss (a ^. aSt ^. ufs) (a ^. aStmt))) <$> as

writeSets :: [AlwaysBlock] -> M
writeSets as = IM.fromList $ (\a -> (a ^. aId, getLhss (a ^. aStmt))) <$> as

makeGraphFromRWSet :: M -> M -> Gr () ()
makeGraphFromRWSet rs ws = mkGraph allNs es
  where
    allNs = 
      fmap (\n -> (n, ())) $
      IS.toList $
      IM.keysSet rs `IS.union` IM.keysSet ws

    es :: [(Int, Int, ())]
    es =
      IM.foldlWithKey'
      (\l n s ->
          -- ns : all blocks that update the sensitivity list of block# n
          let ns = IS.toList $ foldMap (\v -> HM.lookupDefault IS.empty v sensitizers) s
          -- (n1, n2) means n1's block is before after n2 executes
          in ((\n' -> (n',n,())) <$> ns) ++ l
      )
      []
      rs

    -- v: variable ==> {n:block k# | n updates v}
    sensitizers :: M2
    sensitizers =
      let f v Nothing  = Just $ IS.singleton v
          f v (Just s) = Just $ IS.insert v s
      in IM.foldlWithKey'
         (\m n s -> HS.foldl' (\m' v -> HM.alter (f n) v m') m s)
         HM.empty
         ws
