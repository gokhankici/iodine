{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.SMTVCGen ( smtInvs
                                  ) where

import           Control.Lens
import qualified Data.HashMap.Strict      as M

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Solver.SMT.Types
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

smtInvs    :: [AlwaysBlock] -> ([Inv], [InvFun], [UFConst])
smtInvs as = (invs as, invFun <$> as, ufConsts)
  where
    invFun a = InvFun { invFunName  = makeInvPred a
                      , invFunArity = length $ makeInvArgs fmt a
                      }

    ufConsts = M.foldlWithKey' (\ufcs k v -> ufConst k v : ufcs) [] allUFs
    allUFs   = foldr (\a m -> M.union (a^.aSt^.ufs) m) M.empty as

    ufConst n vs = UFConst { ufConstName  = n
                           , ufConstArity = length vs
                           }

