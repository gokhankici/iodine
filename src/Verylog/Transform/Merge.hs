{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.Merge (merge) where

import           Control.Arrow
import           Control.Lens hiding (mapping)
import qualified Data.IntMap.Strict         as IM
import           Data.List
import           Data.Graph.Inductive.Graph hiding ((&))

import           Verylog.Language.Types
import           Verylog.Transform.DFG

-- import Debug.Trace

debug :: String -> a -> a
debug _str n = n
-- debug = trace

merge :: [AlwaysBlock] -> [AlwaysBlock]
merge = mergeAll >>> filterNoRegs

-----------------------------------------------------------------------------------
-- | [AlwaysBlock] -> [AlwaysBlock] :::: Filter out continuous assignments
-----------------------------------------------------------------------------------
mergeAll :: [AlwaysBlock] -> [AlwaysBlock]
mergeAll as = res
  where
    _res = mergeBlocksG abMap g'
    res  = debug ("merge all:\n" ++ intercalate "\n\n" (show . view aStmt <$> _res)) _res

    abMap = IM.fromList $ (\a -> (a ^. aId, a)) <$> as
    rs    = readSets  as
    ws    = writeSets as
    g     = makeGraphFromRWSet abMap rs ws

    g' = if hasCycle g2 then error "mergeAll: has a cycle" else g2

    g2 = let r = breakLoops g
         in  debug (show r) r

    -- try to break loops by removing edges going into @(clock)
    breakLoops :: G -> G
    breakLoops gr =
      let gf (inEdges, n, a, outEdges) =
            let (inEdges', outEdges') =
                  case a of
                    NonBlocking -> ([], outEdges)                  -- remove incoming edges to clocks
                    _           -> (inEdges, filter gf_o outEdges) -- remove edges going into clocks
                gf_o      = h (NonBlocking /=)
                h f       = f . eventToAssignType . (view aEvent) . (abMap IM.!) . snd
            in Just (inEdges', n, a, outEdges')
      in gfiltermap gf gr

--------------------------------------------------------------------------------
filterNoRegs    :: [AlwaysBlock] -> [AlwaysBlock]
--------------------------------------------------------------------------------
filterNoRegs as = filter f as
  where
    f a = let vars = a ^. aSt ^. ports
              check = foldr (\p b -> b ||
                                     case p of
                                       Register _ -> True
                                       _          -> False) False vars
          in if check
             then True
             else debug ("removed unnecessary block:\n" ++ show a) False

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

