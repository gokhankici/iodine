{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Merging (merge) where

import           Control.Arrow
import           Control.Lens hiding (mapping)
-- import           Control.Monad.State.Lazy
import qualified Data.IntMap.Strict         as IM
-- import qualified Data.HashMap.Strict        as HM
-- import qualified Data.HashSet               as HS
import           Data.List

import           Verylog.Language.Types
import           Verylog.Transform.DFG

import Text.Printf
import Debug.Trace

merge :: [AlwaysBlock] -> [AlwaysBlock]
merge =
  mergeClocks
  >>>
  mergeStars 
  >>>
  removeAssigns

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
mergeClocks as = clocks' ++ assigns ++ rest
  where
    (posAs, negAs, assigns, rest) =
      foldl' (\(ps,ns, asns, rs) a ->
                 case a ^. aEvent of
                   PosEdge _ -> (a:ps, ns,   asns,   rs)
                   NegEdge _ -> (ps,   a:ns, asns,   rs)
                   Assign    -> (ps,   ns,   a:asns, rs)
                   Star      -> (ps,   ns,   asns,   a:rs))
      ([],[],[],[]) as

    maxId = maximum $ (view aId) <$> as

    as'   = groups ++ assigns ++ rest
    abMap = IM.fromList $ (\a -> (a^.aId, a)) <$> as'

    clocks' = trace ("merge clocks:\n" ++ intercalate "\n\n" (show . view aStmt <$> merges)) merges
    merges  =
      mergeBlocks abMap (rs,ws)
      where
        rs    = readSets  (groups ++ assigns)
        ws    = writeSets assigns
      

    groups = mergeGroup posAs 1 ++ mergeGroup negAs 2

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
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
removeAssigns :: [AlwaysBlock] -> [AlwaysBlock]
removeAssigns = filter ((/= Assign) . (view aEvent))

-----------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------
mergeBlocks :: AM -> (RWS, RWS) -> [AlwaysBlock]
mergeBlocks abMap (rs,ws) = merges
  where
    merges   :: [AlwaysBlock]
    merges =
      [ let as2      = (abMap IM.!) <$> ns
            a' = AB { _aEvent = (last as2) ^. aEvent
                    , _aStmt  = Block $ view aStmt <$> as2
                    , _aId    = n + maxId
                    , _aSt    = mconcat $ view aSt <$> as2
                    , _aLoc   = ("* join", "* join")
                    }

        in a'
      | (n, ns) <- zip [1..] (pathsToNonAssigns abMap rs ws)
      ]

    maxId = fst $ IM.findMax abMap

