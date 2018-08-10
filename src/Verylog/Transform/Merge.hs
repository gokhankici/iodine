{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Merge (merge) where

import           Verylog.Language.Types
import           Verylog.Transform.DFG

import           Control.Arrow
import           Control.Lens hiding (mapping)
import qualified Data.IntMap.Strict         as IM
-- import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Graph.Inductive.Graph hiding ((&))
import           Control.Exception
import           Text.Printf
import           Debug.Trace

merge :: [AlwaysBlock] -> [AlwaysBlock]
merge =
  mergeClocks
  >>>
  mergeAll
  -- >>>
  -- printBlocks

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
mergeAll :: [AlwaysBlock] -> [AlwaysBlock]
mergeAll as = res
  where
    _res = mergeBlocksG abMap g'
    res  = debug ("merge all:\n" ++ intercalate "\n\n" (show . view aStmt <$> _res)) _res

    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    rs    = readSets as
    ws    = writeSets $ filter ((/=) NonBlocking . eventToAssignType . view aEvent) as
    g     = makeGraphFromRWSet abMap rs ws

    g' = if hasCycle g then error "mergeAll: has a cycle" else g

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

    mergeGroup gs n = if null gs then [] else [a]
      where
        event' = let e = head gs ^. aEvent
                 in  assert (e /= Assign) e

        a = AB { _aEvent = event'
               , _aStmt  = Block $ view aStmt <$> gs
               , _aId    = n + maxId
               , _aSt    = mconcat $ view aSt <$> gs
               , _aLoc   = ("clk join", "clk join")
               }

printBlocks :: [AlwaysBlock] -> [AlwaysBlock]
printBlocks as = f <$> as
  where
    f a = trace (printA a) a

    printA :: AlwaysBlock -> String
    printA a = printf
               -- "block #%d [%s]:\n%s\n%s"
               "block #%d [%s]:\n%s"
               (a^.aId) (show $ a^.aEvent)
               -- (show $ sort $ HM.toList (a^.aSt^.ufs))
               (show (a^.aStmt))

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


mergeBlocksG :: AM -> G -> [AlwaysBlock]
mergeBlocksG abMap g = _mergeBlocks abMap (pathsToNonAssignsG g)

debug :: String -> a -> a
debug str n = if   enabled
              then trace str n
              else n
  where
    enabled = False

