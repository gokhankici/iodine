{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Verylog.Transform.Merge ( merge
                               , Annots
                               ) where

import           Verylog.Language.Types
import           Verylog.Transform.DFG
import           Verylog.Solver.FP.Types           (FPQualifier(..))

import           Control.Arrow
import           Control.Lens               hiding (mapping)
import qualified Data.IntMap.Strict         as IM
import qualified Data.HashSet               as HS
import           Data.List
import           Data.Graph.Inductive.Graph hiding ((&))
import           Control.Exception
import           Text.Printf
import           Debug.Trace

type Annots = ([Id], [FPQualifier])
type ABS    = [AlwaysBlock]

merge :: (ABS, Annots) -> (ABS, Annots)
merge =
  mergeEquals
  >>>
  first
  (
    disable mergeClocks
    >>>
    mergeAssignsAndStars
    >>>
    disable mergeAssigns
    >>>
    disable printBlocks
  )
  where
    disable _ = id

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
mergeEquals :: (ABS, Annots) -> (ABS, Annots)
mergeEquals (as, annots) = (as', annots)
  where
    as' = rest ++ newclocks1 ++ newclocks2
    (_, qualifiers) = annots

    (clocks, rest) = partition ((==) NonBlocking . eventToAssignType . view aEvent) as

    newclocks1 = mergeBlocks abMap iss
    newclocks2 = let toRemove = concat iss
                 in  filter (not . flip elem toRemove . view aId) clocks

    ws  = writeSets clocks
    vss = foldl' (\acc -> \case
                     QualifEqs vs -> vs:acc
                     _            -> acc) [] qualifiers

    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    iss =
      let iss1 =
            [ IM.foldMapWithKey
              (\i s -> if any (flip HS.member s) vs then [i] else [])
              ws
            | vs <- vss
            ]
      in  filter ((> 1) . length) iss1
                     
mergeAssignsAndStars :: [AlwaysBlock] -> [AlwaysBlock]
mergeAssignsAndStars as = res
  where
    _res = mergeBlocksG abMap g'
    res  = debug ("merge all:\n" ++ intercalate "\n\n" (show . view aStmt <$> _res)) _res

    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    rs    = readSets as
    ws    = writeSets $ filter ((/=) NonBlocking . eventToAssignType . view aEvent) as
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

mergeAssigns :: [AlwaysBlock] -> [AlwaysBlock]
mergeAssigns as = as'
  where
    assigns = filter ((==) Continuous . eventToAssignType . view aEvent) as
    rs      = readSets as
    ws      = writeSets assigns
    abMap   = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
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
mergeBlocks :: AM -> [[Node]] -> [AlwaysBlock]
mergeBlocks abMap nss = 
  [ let as2      = (abMap IM.!) <$> ns
        a' = AB { _aEvent = (last as2) ^. aEvent
                , _aStmt  = Block $ view aStmt <$> as2
                , _aId    = n + maxId
                , _aSt    = mconcat $ view aSt <$> as2
                , _aLoc   = ("mergeBlocks", "mergeBlocks")
                }
        ns = debug (show ns') ns'

    in a'
  | (n, ns') <- zip [1..] nss
  ]
  where
    maxId = fst $ IM.findMax abMap


mergeBlocksG :: AM -> G -> [AlwaysBlock]
mergeBlocksG abMap g = mergeBlocks abMap (pathsToNonAssignsG g)

debug :: String -> a -> a
debug str n = if   enabled
              then trace str n
              else n
  where
    enabled = False

