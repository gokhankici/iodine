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
import           Data.List

import           Verylog.Language.Types
import           Verylog.Transform.DFG

import Text.Printf
import Debug.Trace

flatten :: St -> [AlwaysBlock]
flatten st =
  let res = flattenToAlways >>>
            mergeStars >>>
            mergeClocks >>>
            removeAssigns $ st
      s   = intercalate "\n\n" $
            (\a -> printf
                   "block #%d:\n%s\n%s"
                   (a^.aId)
                   (show $ sort $ HM.toList (a^.aSt^.ufs))
                   (show (a^.aStmt))) <$>
            res
  in  trace (printf "as(#%d):\n%s" (length res) s) res

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


------------------------------------------------------------------------------------
-- | mergeStars :: [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks with @(*)
------------------------------------------------------------------------------------
mergeStars :: [AlwaysBlock] -> [AlwaysBlock]
mergeStars as = stars' ++ assigns ++ others
  where
    (stars, assigns, others) =
      foldl' (\(ss,asns, os) a ->
                case a ^. aEvent of
                  Star   -> (a:ss, asns,   os)
                  Assign -> (ss,   a:asns, os)
                  _      -> (ss,   asns,   a:os))
      ([],[],[]) as

    stars' = trace ("merge stars:\n" ++ intercalate "\n\n" (show . view aStmt <$> merges)) merges

    merges   :: [AlwaysBlock]
    merges =
      mconcat
      [ let as2               = (abMap IM.!) <$> ns
            (_, as3) = span ((== Assign) . view aEvent) $ reverse as2
            as'               = reverse as3

            a' = AB { _aEvent = Star
                    , _aStmt  = Block $ view aStmt <$> as'
                    , _aId    = n + maxId
                    , _aSt    = mconcat $ view aSt <$> as'
                    , _aLoc   = ("* join", "* join")
                    }

            hasStar    = not . null $ filter ((== Star).(view aEvent)) as'

        in if   hasStar then [a'] else []
      | (n, ns) <- zip [1..] (pathsToNonAssigns abMap rs ws)
      ]

    -- rs: block # ==> sensitivity list
    -- ws: block # ==> update list
    rs = readSets  (stars ++ assigns)
    ws = writeSets (stars ++ assigns)

    abMap :: AM
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
          a' = trace (printf "merged clocks:\n%s" (show $ a ^. aStmt)) a
          
      in case gs' of
           [] -> gs
           _  -> a' : rs

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks to remove wires from invariants
-----------------------------------------------------------------------------------
removeAssigns :: [AlwaysBlock] -> [AlwaysBlock]
removeAssigns = filter ((/= Assign) . (view aEvent))

-- removeWires :: [AlwaysBlock] -> [AlwaysBlock]
-- removeWires as = [ mkAB (n+maxId) ns | (n,ns) <- zip [1..] (pathsToNonAssigns globalG) ]
--   where
--     assigns = filter ((== Assign).(view aEvent)) as

--     aMap :: AM
--     aMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as

--     maxId = fst $ IM.findMax aMap
    
--     readsMap  = readSets as
--     writesMap = writeSets assigns

--     globalG = makeGraphFromRWSet aMap readsMap writesMap
  
--     mkAB :: Int -> [Int] -> AlwaysBlock
--     mkAB n is = 
--       let mergedAs = (aMap IM.!) <$> is
--           lastA    = last mergedAs
--       in AB { _aEvent = lastA ^. aEvent
--             , _aStmt  = Block $ view aStmt <$> mergedAs
--             , _aId    = n
--             , _aSt    = mconcat $ view aSt <$> mergedAs
--             , _aLoc   = ("clk join", "clk join")
--             }

