{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.Merge ( merge
                               ) where

import           Iodine.Types
import           Iodine.Language.Types
import           Iodine.Transform.DFG
import           Iodine.Solver.FP.Types

import           Control.Arrow
import           Control.Exception
import           Control.Lens               hiding (mapping)
import qualified Data.IntMap.Strict         as IM
import qualified Data.HashSet               as HS
import           Data.List
import           Data.Graph.Inductive.Graph hiding ((&))
import           Text.Printf
import           Debug.Trace

import qualified Data.Sequence as SQ
import qualified Data.Foldable as F

type AI = ABS Id
type Qs = Qualifiers Id

merge :: (AI, Qs) -> AI
merge =
  mergeEquals
  >>>
  fst
  >>>
  disable mergeClocks
  >>>
  mergeAssignsAndStars
  >>>
  disable mergeAssigns
  >>>
  disable printBlocks
  where
    disable :: a -> b -> b
    disable _ = id

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
mergeEquals :: (AI, Qs) -> (AI, Qs)
mergeEquals (as, allQualifiers) = (as', allQualifiers)
  where
    as' = rest SQ.>< newclocks1 SQ.>< newclocks2

    (clocks, rest) = SQ.partition ((==) NonBlocking . eventToAssignType . view aEvent) as

    newclocks1 = mergeBlocks abMap iss
    newclocks2 = let toRemove = concat iss
                 in  SQ.filter (not . flip elem toRemove . view aId) clocks

    ws  = writeSets clocks
    vss = foldl' (\acc -> \case
                     QualifPairs vs -> vs:acc
                     _              -> acc) [] allQualifiers

    abMap = abs2Map as
    iss =
      let iss1 =
            [ IM.foldMapWithKey
              (\i s -> if any (flip HS.member s) vs then [i] else [])
              ws
            | vs <- vss
            ]
      in  filter ((> 1) . length) iss1
                     
mergeAssignsAndStars :: AI -> AI
mergeAssignsAndStars as = res
  where
    _res = mergeBlocksG abMap g'
    res  = debug ("merge all:\n" ++ intercalate "\n\n" (show . view aStmt <$> (F.toList _res))) _res

    abMap = abs2Map as
    rs    = readSets as
    ws    = writeSets $ SQ.filter ((/=) NonBlocking . eventToAssignType . view aEvent) as
    g     = makeGraphFromRWSet abMap rs ws

    g' = if hasCycle g then error "mergeAssignsAndStars: has a cycle" else g

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

    mergeGroup :: [AlwaysBlock] -> Int -> [AlwaysBlock]
    mergeGroup gs n = if null gs then [] else [a]
      where
        event' = let e = head gs ^. aEvent
                 in  assert (e /= Assign) e

        a = AB { _aEvent   = event'
               , _aStmt    = Block $ view aStmt <$> gs
               , _aId      = n + maxId
               , _aSt      = mconcat $ view aSt <$> gs
               , _aLoc     = ("clk join", "clk join")
               , _aMd      = mconcat $ view aMd <$> gs
               }

mergeAssigns :: AI -> AI
mergeAssigns as = as'
  where
    assigns = SQ.filter ((==) Continuous . eventToAssignType . view aEvent) as
    rs      = readSets as
    ws      = writeSets assigns
    abMap   = abs2Map as
    g       = makeGraphFromRWSet abMap rs ws
    g'      = if hasCycle g then error "mergeAssigns: has a cycle" else g
    as'     = mergeBlocksG abMap g'
  

printBlocks :: [AlwaysBlock] -> [AlwaysBlock]
printBlocks as = f <$> as
  where
    f a = trace (printA a) a

    printA :: AlwaysBlock -> String
    printA a = printf
               -- "block #%d [%s]:\n%s\n%s"
               "block #%d [%s]:\n%s\n"
               (a^.aId) (show $ a^.aEvent)
               -- (show $ sort $ HM.toList (a^.aSt^.ufs))
               (show (a^.aStmt))

-----------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------
mergeBlocks :: AM -> [[Node]] -> AI
mergeBlocks abMap nss = go (1, mempty) nss
  where
    maxId = fst $ IM.findMax abMap

    go :: (Int, AI) -> [[Node]] -> AI
    go (_, acc) [] = acc
    go (!n, !acc) (ns':rest) =
      let as2 = (abMap IM.!) <$> ns
          a'  = AB { _aEvent = (last as2) ^. aEvent
                   , _aStmt  = Block $ view aStmt <$> as2
                   , _aId    = n + maxId
                   , _aSt    = mconcat $ view aSt <$> as2
                   , _aLoc   = ("mergeBlocks", "mergeBlocks")
                   , _aMd    = mconcat $ view aMd <$> as2
                   }
          ns  = debug (show ns') ns'
     in go (n+1, acc SQ.|> a') rest

mergeBlocksG :: AM -> G -> AI
mergeBlocksG abMap g = mergeBlocks abMap (pathsToNonAssignsG g)

debug :: String -> a -> a
debug str n = if   enabled
              then trace str n
              else n
  where
    enabled = False

abs2Map :: AI -> IM.IntMap AlwaysBlock
abs2Map = F.foldl' (\acc a -> IM.insert (a^.aId) a acc) mempty
