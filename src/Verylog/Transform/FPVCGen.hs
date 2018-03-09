{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.FPVCGen ( fpInvs
                                 ) where

import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashMap.Strict      as M

import           Verylog.Language.Types 
import           Verylog.Solver.Common
import           Verylog.Solver.FP.Types hiding (invs, ufs)
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

import Data.List (intercalate)
import Text.Printf

fpInvs    :: [AlwaysBlock] -> FPSt
fpInvs as = FPSt { _constraints = cs
                 , _invs        = ifs
                 , _ufs         = ufConsts
                 , _binds       = getBinds cs ifs
                 }
  where
    cs  = invs as
    ifs = invFun <$> as
    invFun a = InvFun { invFunName  = makeInvPred a
                      , invFunArity = length $ makeInvArgs fmt a
                      }

    ufConsts = M.foldlWithKey' (\ufcs k v -> ufConst k v : ufcs) [] allUFs
    allUFs   = foldr (\a m -> M.union (a^.aSt^.ufs) m) M.empty as

    ufConst n vs = UFConst { ufConstName  = n
                           , ufConstArity = length vs
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
getBindsFromExp (Structure _ as) = getBindsFromExps (Var <$> as)
getBindsFromExp (Var v)          = do
  has <- uses _2 (M.member v)
  when (not has) $ do
    n' <- use _1; _1 += 1
    _2 %= M.insert v (FQBind { bindId   = n'
                             , bindName = v
                             , bindType = "int"
                             , bindRef  = "true"
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
                                  , bindRef  = "true"
                                  })
    makeUFType n =
      let argsStr = intercalate "," (replicate n "Int") 
          argTup  = if n > 1 then printf "(%s)" argsStr else argsStr
      in if   n > 0
         then printf "Map_t %s Int" argTup
         else "int"

    addSel :: Id -> [Id] -> S ()
    addSel name args = do
      let selIn  = printf "%s" (intercalate "," args) :: String
          selTup = if length args > 1 then printf "(%s)" selIn else selIn
          selRef = if   length args > 0
                   then printf "v = Map_select %s %s" ufFunc selTup
                   else printf "v = %s" ufFunc
          
      n' <- use _1; _1 += 1
      _2 %= M.insert name (FQBind { bindId   = n'
                                  , bindName = name
                                  , bindType = "int"
                                  , bindRef  = selRef
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
                                      , bindType = "int"
                                      , bindRef  = "true"
                                      }
            args = tail $ argVars inv
            m' = foldr
                 (\(argName,n1) m ->
                     M.insert argName (binder argName (n1+n)) m)
                 m
                 args
            n' = n + invFunArity
        in  (n', m')
  put $ foldr addInvArgs (n, m) invs
