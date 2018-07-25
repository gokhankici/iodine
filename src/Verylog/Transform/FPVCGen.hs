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
-- import Debug.Trace

toFpSt    :: ([AlwaysBlock], ([Id],[FPQualifier])) -> FPSt
toFpSt (as, (srcs,qs)) =
  FPSt { _fpConstraints = cs
       , _fpABs         = as
       , _fpBinds       = bs -- trace (show $ M.keys bs) bs
       , _fpUFs         = M.unions $ (view (aSt . ufs)) <$> as
       , _fpQualifiers  = qs
       }
  where
    cs   = invs srcs as
    bs   = getBinds as cs


type S = State (Int, BindMap)

getBinds       :: [AlwaysBlock] -> [Inv] -> BindMap
getBinds as cs = evalState comp (length constants + 1, m)
  where
    comp = do sequence_ (getBind <$> cs)
              let is = (makeInvArgs fmt <$> as) -- ++ (makeInvArgs fmt{primedVar=True} <$> as)
              getBindsFromExps $ map Var $ concat is
              use _2

    m = foldr constBind M.empty (zip [1..] constants)
    h (Number n)      = FQ.ECon (FQ.I (toInteger n))
    h (Boolean True)  = FQ.PTrue
    h (Boolean False) = FQ.PFalse
    h _               = error "oh no!"
    constBind (i, (name, e)) =
      M.insert name FQBind { bindId   = i
                           , bindName = name
                           , bindType = case e of
                                          Boolean _ -> FQ.FTC FQ.boolFTyCon
                                          _         -> FQ.FInt
                           , bindRef  = (case e of
                                          Boolean _ -> FQ.PIff
                                          _         -> FQ.PAtom FQ.Eq)
                                        (FQ.EVar $ FQ.symbol "v")
                                        (h e)
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
  bindingExists <- uses _2 (M.member v)
  when (not bindingExists) $ do
    n' <- use _1; _1 += 1
    _2 %= M.insert v (FQBind { bindId   = n'
                             , bindName = v
                             , bindType = if isTag v then FQ.FTC FQ.boolFTyCon else FQ.FInt
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
