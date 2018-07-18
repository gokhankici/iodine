{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.DFG (wireTaints) where

import           Control.Lens                       hiding (mapping)
import qualified Data.HashSet                       as HS
import qualified Data.HashMap.Strict                as HM
import           Verylog.Language.Types
-- import           Verylog.Transform.Utils

type M = HM.HashMap Id [Id]
type S = HS.HashSet Id

--------------------------------------------------------------------------------
wireTaints :: AlwaysBlock -> [Id] -> [Id]
--------------------------------------------------------------------------------
-- takes an always block and a list of wires, and returns the registers that
-- should be tainted by the analysis at the initial state
wireTaints a srcs = HS.toList $ worklist a assignments srcs
  where
    assignments = stmt2Assignments (a ^. aStmt) (a ^. aSt ^. ufs)

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
                           Just l  -> l
              wrklst'  = foldr (\rhs wl' -> if   HS.member rhs donelst'
                                            then wl'
                                            else rhs:wl') wrklst_ rhss
          in h (wrklst', donelst', reglst')

    regs = foldr (\p s -> case p of
                            Register r -> HS.insert r s
                            _          -> s)
           HS.empty (a ^. aSt ^. ports)

stmt2Assignments :: Stmt -> M -> M
stmt2Assignments s unintFuncs = h [] s
  where
    h :: [Id] -> Stmt -> M
    h _ Skip                  = HM.empty
    h l (BlockingAsgn{..})    = h2 (l2ls lhs ++ l) rhs HM.empty 
    h l (NonBlockingAsgn{..}) = h2 (l2ls lhs ++ l) rhs HM.empty 
    h l (IfStmt{..})          = HM.unionWith (++)
                                (h (l2ls ifCond ++ l) thenStmt)
                                (h (l2ls ifCond ++ l) elseStmt)
    h l (Block{..})           = foldr (HM.unionWith (++)) HM.empty (h l <$> blockStmts)

    h2 :: [Id] -> Id -> M -> M
    h2 ls r m = foldr (\l m' -> HM.alter (\ml -> case ml of
                                             Nothing -> Just [r]
                                             Just rs -> Just (r:rs)
                                         ) l m') m ls

    l2ls   :: Id -> [Id]
    l2ls l = case HM.lookup l unintFuncs of
               Nothing -> [l]
               Just ls -> ls