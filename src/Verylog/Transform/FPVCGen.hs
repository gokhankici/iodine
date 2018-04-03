{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.FPVCGen ( toFpSt
                                 ) where

import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashMap.Strict      as M
import qualified Language.Fixpoint.Types  as FQ

import           Verylog.Language.Types 
import           Verylog.Solver.Common
import           Verylog.Solver.FP.Types
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

toFpSt    :: [AlwaysBlock] -> FPSt
toFpSt as = FPSt { _fpConstraints = cs
                 , _fpABs         = as
                 , _fpBinds       = getBinds cs
                 , _fpUFs         = M.unions $ (M.map length) . (view (aSt . ufs)) <$> as
                 }
  where
    cs   = invs as


type S = State (Int, BindMap)

getBinds    :: [Inv] -> BindMap
getBinds is = evalState comp (length constants + 1, m)
  where
    comp = do sequence_ (getBind <$> is)
              use _2

    m = foldr constBind M.empty (zip [1..] constants)
    constBind (i, (name, val)) =
      M.insert name FQBind { bindId   = i
                           , bindName = name
                           , bindType = FQ.FInt
                           , bindRef  = FQ.PAtom FQ.Eq
                                        (FQ.EVar $ FQ.symbol "v")
                                        (FQ.ECon $ FQ.I val)
                           }

getBind            :: Inv -> S ()
getBind (Horn{..}) = getBindsFromExps [hBody, hHead]

getBindsFromExp :: Expr -> S ()
getBindsFromExp (BinOp{..}) = getBindsFromExps [expL, expR]
getBindsFromExp (Ands es)   = getBindsFromExps es
getBindsFromExp (Ite{..})   = getBindsFromExps [cnd, expThen, expElse]
getBindsFromExp (KV{..})    = getBindsFromExps (Var <$> vs) >> getBindsFromExps es
  where
    (vs,es) = unzip kvSubs
getBindsFromExp (Var v)     = do
  has <- uses _2 (M.member v)
  when (not has) $ do
    n' <- use _1; _1 += 1
    _2 %= M.insert v (FQBind { bindId   = n'
                             , bindName = v
                             , bindType = FQ.FInt
                             , bindRef  = FQ.prop True
                             })
getBindsFromExp (UFCheck{..})    =
  getBindsFromExps $ uncurry (++) $ unzip ufArgs
getBindsFromExp (Number _)       = return ()
getBindsFromExp (Boolean _)      = return ()

getBindsFromExps :: [Expr] -> S ()
getBindsFromExps = sequence_ . (map getBindsFromExp)

-- addArgs invs = do
--   n <- use _1
--   m <- use _2
--   let addInvArgs inv@(InvFun{..}) (n,m) =
--         let binder argName n = FQBind { bindId   = n
--                                       , bindName = argName
--                                       , bindType = FQ.FInt
--                                       , bindRef  = FQ.prop True
--                                       }
--             args = argVars inv
--             m' = foldr
--                  (\(argName,n1) m ->
--                      M.insert argName (binder argName (n1+n)) m)
--                  m
--                  args
--             n' = n + invFunArity
--         in  (n', m')
--   let (n', m') = foldr addInvArgs (n, m) invs
--   _1 .= n'
--   _2 .= m'
