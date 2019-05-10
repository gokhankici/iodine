{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Iodine.Transform.DFG
  ( wireTaints
  , assignmentMap
  , stmt2Assignments

  , AM
  , RWS
  , G
  , AssignType(..)
  , hasCycle
  , readSets
  , writeSets
  , pathsToNonAssigns
  , pathsToNonAssignsG
  , makeGraphFromRWSet
  , eventToAssignType
  ) where

import           Iodine.Language.Types
import           Iodine.Transform.Utils

import           Control.Lens        hiding (mapping, pre)
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
import           Text.Printf
import           Data.Monoid
import qualified Data.Sequence       as SQ
import qualified Data.Foldable       as F

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query hiding (trc)

--------------------------------------------------------------------------------
wireTaints :: AlwaysBlock -> [Id] -> [Id]
--------------------------------------------------------------------------------
-- takes an always block and a list of wires/registers, and returns the registers that
-- should be tainted by the analysis at the initial state
wireTaints a srcs =  rs ++ (HS.toList $ worklist a (assignmentMap a) ws)
  where
    (rs,ws) = let f p (rss,wss) =
                    let l :: SQ.Seq Var = SQ.filter (\p' -> varName p' == p) prts
                    in case SQ.viewl l of
                         SQ.EmptyL            -> error $ printf "cannot find %s in ports" p
                         (Register _) SQ.:< _ -> (p:rss, wss)
                         (Wire _)     SQ.:< _ -> (rss, p:wss)
              in  foldr f ([],[]) srcs
    prts    = a ^. aSt ^. ports

assignmentMap :: AlwaysBlock -> M
assignmentMap a = stmt2Assignments (a ^. aStmt)

worklist :: AlwaysBlock -> M -> [Id] -> S
worklist a assignments wl = h (wl, HS.empty, HS.empty)
  where
    h :: ([Id], S, S) -> S
    h (wrklst, donelst, reglst) =
      case wrklst of
        []        -> reglst
        p:wrklst_ ->
          let donelst' = HS.insert p donelst
              reglst'  = if   HS.member p regs
                         then HS.insert p reglst
                         else reglst
              rhss     = case HM.lookup p assignments of
                           Nothing -> []
                           Just l  -> HS.toList l
              wrklst'  = foldr (\rhs wl' -> if   HS.member rhs donelst
                                            then wl'
                                            else rhs:wl') wrklst_ rhss
          in h (wrklst', donelst', reglst')

    regs = foldr (\p s -> case p of
                            Register r -> HS.insert r s
                            _          -> s)
           HS.empty (a ^. aSt ^. ports)

stmt2Assignments :: Stmt -> M
stmt2Assignments s = h mempty s
  where
    h :: IdSeq -> Stmt -> M
    h _ Skip                  = HM.empty
    h l (BlockingAsgn{..})    = h2 (l2ls rhs SQ.>< l) lhs HM.empty
    h l (NonBlockingAsgn{..}) = h2 (l2ls rhs SQ.>< l) lhs HM.empty
    h l (IfStmt{..})          = HM.unionWith HS.union
                                (h (l2ls ifCond SQ.>< l) thenStmt)
                                (h (l2ls ifCond SQ.>< l) elseStmt)
    h l (Block{..})           = foldr (HM.unionWith HS.union) HM.empty (h l <$> blockStmts)

    h2 :: IdSeq -> Id -> M -> M
    h2 ls r m = F.foldl' (\m' l -> HM.alter
                           (\ml -> case ml of
                                     Nothing -> Just $ HS.singleton r
                                     Just rs -> Just $ HS.insert r rs
                           ) l m'
                         ) m ls

    l2ls :: VExpr -> IdSeq
    l2ls = vexprPortSeq


type S = HS.HashSet Id
type M = HM.HashMap Id S

type RWS = IM.IntMap S

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data AssignType = Continuous
                | Blocking
                | NonBlocking
                deriving (Show, Eq)

type G  = Gr AssignType ()
type AM = IM.IntMap AlwaysBlock
type M2 = HM.HashMap Id IS.IntSet

hasCycle :: Gr a b -> Bool
hasCycle g = any ((>= 2) . length) (scc g)

readSets :: SQ.Seq AlwaysBlock -> RWS
readSets as = F.foldl' (\acc a -> IM.insert (a^.aId) (getRhss (a^.aStmt)) acc) mempty as


writeSets :: SQ.Seq AlwaysBlock -> RWS
writeSets as = F.foldl' (\acc a -> IM.insert (a^.aId) (getLhss (a^.aStmt)) acc) mempty as

pathsToNonAssignsG :: G -> [[Node]]
pathsToNonAssignsG g =
  [ let g'' = subgraph c g'
    in  reverse $ topsort g''
  | c <- components g'
  ]
  where
    g' = removeAssignRoots g

pathsToNonAssigns :: AM -> RWS -> RWS -> [[Node]]
pathsToNonAssigns abMap rs ws = pathsToNonAssignsG g
  where
    g = let res = makeGraphFromRWSet abMap rs ws
        in  if   hasCycle res
            then error "pathsToNonAssigns: graph has a cycle !"
            else res

removeAssignRoots :: G -> G
removeAssignRoots gr =
  let nNodes        = order gr
      gAfterRemoval = gfiltermap remIfAsgnRoot gr
      nNodes'       = order gAfterRemoval
  in if   nNodes == nNodes'
     then gr
     else removeAssignRoots gAfterRemoval

  where
    remIfAsgnRoot ctx@(_, n, asgnT, _) =
      let inEdges = pre gr n
      in if   inEdges == [] && asgnT == Continuous -- is an cont. assign root
         then debug ("removed:" ++ show ctx) Nothing
         else Just ctx


-- | (n1, n2) \in edges means n1 reads from a variable that n2 writes to
makeGraphFromRWSet :: AM -> RWS -> RWS -> G
makeGraphFromRWSet abMap rs ws = mkGraph allNs es
  where
    allNs =
      fmap h $
      IS.toList $
      IM.keysSet rs `IS.union` IM.keysSet ws

    h n =
      let a = abMap IM.! n
      in  debug (show (n,  a ^. aStmt)) $ (n, aId2AsgnT n)

    aId2AsgnT :: Int -> AssignType
    aId2AsgnT n = let a = abMap IM.! n
                  in  eventToAssignType (a ^. aEvent)

    es :: [(Int, Int, ())]
    es =
      IM.foldlWithKey'
      (\l n s ->
          -- ns : all blocks that update the sensitivity list of block# n
          let ns  = IS.toList $ foldMap (\v -> HM.lookupDefault IS.empty v sensitizers) s
              ns' = filter (n /=) ns
          -- (n1, n2) means n1's block is ***after*** after n2 executes
          in ((\n' -> (n,n',())) <$> ns') ++ l
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

getRhss :: Stmt -> S
getRhss = go
  where
    go Skip                  = HS.empty
    go (BlockingAsgn{..})    = vexprPortSet rhs
    go (NonBlockingAsgn{..}) = vexprPortSet rhs
    go (IfStmt{..})          = vexprPortSet ifCond <> foldMap go [thenStmt, elseStmt]
    go (Block{..})           = foldMap go blockStmts

eventToAssignType :: EventA a -> AssignType
eventToAssignType Assign      = Continuous
eventToAssignType Star        = Blocking
eventToAssignType (PosEdge _) = NonBlocking
eventToAssignType (NegEdge _) = NonBlocking

debug :: String -> a -> a
-- debug = trace
debug _str a = a
