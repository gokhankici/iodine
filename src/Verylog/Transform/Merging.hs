{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Merging (merge) where

import           Control.Arrow
import           Control.Lens hiding (mapping)
import qualified Data.IntMap.Strict         as IM
import           Data.List
import           Data.Graph.Inductive.Graph hiding ((&))

import           Verylog.Language.Types
import           Verylog.Transform.DFG

import Text.Printf
-- import Debug.Trace

debug :: String -> a -> a
debug _str n = n
-- debug = trace

merge :: [AlwaysBlock] -> [AlwaysBlock]
merge =
  mergeClocks
  >>>
  mergeAll
  -- >>>
  -- mergeStars 
  -- >>>
  -- removeAssigns

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

    stars' = debug ("merge stars:\n" ++ intercalate "\n\n" (show . view aStmt <$> merges)) merges

    merges =
      mergeBlocks abMap (rs,ws)
      where
        rw_as = stars ++ assigns
        rs    = readSets  rw_as
        ws    = writeSets rw_as

    abMap :: AM
    abMap = IM.fromList $ (\a -> (a^.aId, a)) <$> as
  
-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Merge always blocks with @(clock)
-----------------------------------------------------------------------------------

mergeClocks :: [AlwaysBlock] -> [AlwaysBlock]
mergeClocks as = groups ++ assigns ++ rest
  where
    groups = mergeGroup posAs 1 ++ mergeGroup negAs 2

    (posAs, negAs, assigns, rest) =
      foldl' (\(ps,ns, asns, rs) a ->
                 case a ^. aEvent of
                   PosEdge _ -> (a:ps, ns,   asns,   rs)
                   NegEdge _ -> (ps,   a:ns, asns,   rs)
                   Assign    -> (ps,   ns,   a:asns, rs)
                   Star      -> (ps,   ns,   asns,   a:rs))
      ([],[],[],[]) as

    maxId = maximum $ (view aId) <$> as

    -- as'   = groups ++ assigns ++ rest
    -- abMap = IM.fromList $ (\a -> (a^.aId, a)) <$> as'

    -- clocks' = debug ("merge clocks:\n" ++ intercalate "\n\n" (show . view aStmt <$> merges)) merges
    -- merges  =
    --   mergeBlocks abMap (rs,ws)
    --   where
    --     rs    = readSets  (groups ++ assigns)
    --     ws    = writeSets assigns
      
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
          a' = debug (printf "merged clocks:\n%s" (show $ a ^. aStmt)) a
          
      in case gs' of
           [] -> gs
           _  -> a' : rs

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
mergeAll :: [AlwaysBlock] -> [AlwaysBlock]
mergeAll as = res
  where
    _res = mergeBlocksAll
    res  = debug ("merge all:\n" ++ intercalate "\n\n" (show . view aStmt <$> _res)) _res

    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    rs    = readSets  as
    ws    = writeSets as
    g     = makeGraphFromRWSet abMap rs ws

    g2 = let r = breakLoops g
         in  debug (show r) r

    g'  =
      if   hasCycle g2 then error "mergeAll: has a cycle" else g2
    
    breakLoops :: G -> G
    breakLoops gr =
      let gf (inEdges, n, a, outEdges) =
            let outEdges' = filter gf_o outEdges
                gf_o      = (NonBlocking /=) . eventToAssignType . (view aEvent) . (abMap IM.!) . snd
                inEdges'  = case a of
                              NonBlocking -> []
                              _           -> inEdges
            in Just (inEdges', n, a, outEdges')
      in gfiltermap gf gr

    mergeBlocksAll = mergeBlocksG abMap g'


-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
removeAssigns :: [AlwaysBlock] -> [AlwaysBlock]
removeAssigns = filter ((/= Assign) . (view aEvent))

-----------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------
_mergeBlocks :: AM -> [[Node]] -> [AlwaysBlock]
_mergeBlocks abMap nss = 
  [ let as2      = (abMap IM.!) <$> ns
        a' = AB { _aEvent = (last as2) ^. aEvent
                , _aStmt  = Block $ view aStmt <$> as2
                , _aId    = n + maxId
                , _aSt    = mconcat $ view aSt <$> as2
                , _aLoc   = ("* join", "* join")
                }
        ns = debug (show ns') ns'

    in a'
  | (n, ns') <- zip [1..] nss
  ]
  where
    maxId = fst $ IM.findMax abMap


mergeBlocks :: AM -> (RWS, RWS) -> [AlwaysBlock]
mergeBlocks abMap (rs,ws) = _mergeBlocks abMap nss
  where
    nss   = pathsToNonAssigns abMap rs ws

mergeBlocksG :: AM -> G -> [AlwaysBlock]
mergeBlocksG abMap g = _mergeBlocks abMap (pathsToNonAssignsG g)

