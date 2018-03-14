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
                 , _fpInvs        = ifs
                 , _fpBinds       = getBinds cs ifs
                 , _fpUFs         = M.unions $ (M.map length) . (view (aSt . ufs)) <$> as
                 }
  where
    cs  = invs as
    ifs = invFun <$> as
    invFun a = InvFun { invFunName  = makeInvPred a
                      , invFunArity = length $ makeInvArgs fmt a
                      }


type S = State (Int, BindMap)

getBinds       :: [Inv] -> [InvFun] -> BindMap
getBinds is fs = evalState comp (0, M.empty)
  where
    comp = do sequence_ (getBind <$> is)
              addArgs fs
              use _2

getBind            :: Inv -> S ()
getBind (Prop{..}) = getBindsFromExps [propL, propR]
getBind (Inv{..})  = getBindsFromExp invBody

getBindsFromExp :: Expr -> S ()
getBindsFromExp (BinOp{..})      = getBindsFromExps [expL, expR]
getBindsFromExp (Ands es)        = getBindsFromExps es
getBindsFromExp (Ite{..})        = getBindsFromExps [cnd, expThen, expElse]
getBindsFromExp (Structure{..})  = getBindsFromExps (Var <$> propArgs)
getBindsFromExp (Var v)          = do
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

addArgs :: [InvFun] -> S ()
addArgs invs = do
  n <- use _1
  m <- use _2
  let addInvArgs inv@(InvFun{..}) (n,m) =
        let binder argName n = FQBind { bindId   = n
                                      , bindName = argName
                                      , bindType = FQ.FInt
                                      , bindRef  = FQ.prop True
                                      }
            args = argVars inv
            m' = foldr
                 (\(argName,n1) m ->
                     M.insert argName (binder argName (n1+n)) m)
                 m
                 args
            n' = n + invFunArity
        in  (n', m')
  let (n', m') = foldr addInvArgs (n, m) invs
  _1 .= n'
  _2 .= m'
