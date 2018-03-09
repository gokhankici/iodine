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
                 , _invs        = invFun <$> as
                 , _ufs         = ufConsts
                 , _binds       = getBinds cs
                 }
  where
    cs = invs as
    invFun a = InvFun { invFunName  = makeInvPred a
                      , invFunArity = length $ makeInvArgs fmt a
                      }

    ufConsts = M.foldlWithKey' (\ufcs k v -> ufConst k v : ufcs) [] allUFs
    allUFs   = foldr (\a m -> M.union (a^.aSt^.ufs) m) M.empty as

    ufConst n vs = UFConst { ufConstName  = n
                           , ufConstArity = length vs
                           }


type S = State (Int, (M.HashMap Id FQBind))

getBinds    :: [Inv] -> M.HashMap Id FQBind
getBinds is = evalState (sequence_ (getBind <$> is) >> use _2) (0, M.empty)

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
      if   n > 0
      then printf "v = Map_t (%s) Int" (intercalate "," (replicate n "Int"))
      else "int"

    addSel :: Id -> [Id] -> S ()
    addSel name args = do
      let selIn  = printf "(%s)" (intercalate "," args) :: String
          selRef = if   length args > 0
                   then printf "v = Map_select %s %s" ufFunc selIn 
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

