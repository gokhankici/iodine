{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.FPVCGen ( toFpSt
                                 ) where

import           Control.Monad.State.Lazy
import           Control.Lens
import           Data.List
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
                 }
  where
    cs  = invs as
    ifs = invFun <$> as
    invFun a = InvFun { invFunName  = makeInvPred a
                      , invFunArity = length $ makeInvArgs fmt a
                      }


type S = State (Int, (M.HashMap Id FQBind))

getBinds       :: [Inv] -> [InvFun] -> M.HashMap Id FQBind
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
getBindsFromExp (UFCheck{..})    = do
  getBindsFromExps $ uncurry (++) $ unzip ufArgs
  let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
      (n1,n2)   = ufNames & both %~ idFromExp
      arity     = length ufArgs
  addUf ufFunc arity
  addSel n1 as1
  addSel n2 as2
  where
    addUf            :: Id -> Int -> S ()
    addUf name arity = do
      n' <- use _1; _1 += 1
      _2 %= M.insert name (FQBind { bindId   = n'
                                  , bindName = name
                                  , bindType = makeUFType arity
                                  , bindRef  = FQ.prop True
                                  })
    makeUFType n =
      if   n > 0
      then FQ.mapSort FQ.FInt (makeUFType (n-1))
      else FQ.FInt

    addSel :: Id -> [Id] -> S ()
    addSel name args = do
      let selRef = if   length args > 0
                   then let (v1:vs) = FQ.eVar <$> args
                            selF    = FQ.dummyLoc (FQ.symbol "Map_select")
                            base    = FQ.mkEApp selF [FQ.eVar ufFunc, v1]
                            f acc v = FQ.mkEApp selF [acc, v]
                        in  foldl' f base vs
                   else FQ.eVar ufFunc
          
      n' <- use _1; _1 += 1
      _2 %= M.insert name (FQBind { bindId   = n'
                                  , bindName = name
                                  , bindType = FQ.FInt
                                  , bindRef  = FQ.PAtom FQ.Eq (FQ.eVar "v") selRef
                                  })
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
  put $ foldr addInvArgs (n, m) invs
