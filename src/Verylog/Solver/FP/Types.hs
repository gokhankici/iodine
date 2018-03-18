{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Verylog.Solver.FP.Types
  ( FQBind(..)
  , InvFun(..)
  , UFConst(..)
  , BindMap
  , UFMap

  , FPSt(..)
  , fpConstraints
  , fpInvs
  , fpBinds
  , fpUFs

  , idFromExp
  , argVars
  , argVars'
  ) where

import           Control.Exception
import           Control.Lens
import           Text.Printf
import qualified Data.HashMap.Strict        as M
import qualified Language.Fixpoint.Types    as FQ

import           Verylog.Language.Types hiding (St, ufs)
import           Verylog.Solver.Common

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     , bindType :: FQ.Sort
                     , bindRef  :: FQ.Expr
                     }

data InvFun = InvFun { invFunName   :: Id
                     , invFunArity  :: Int
                     , invFunParams :: [Id]
                     }

data UFConst = UFConst { ufConstName  :: Id
                       , ufConstArity :: Int
                       }
 
type BindMap = M.HashMap Id FQBind
type UFMap   = M.HashMap Id Int

data FPSt = FPSt { _fpConstraints :: [Inv]
                 , _fpInvs        :: [InvFun]
                 , _fpBinds       :: BindMap
                 , _fpUFs         :: UFMap
                 }

makeLenses ''FPSt

idFromExp :: Expr -> Id
idFromExp (Var v) = v
idFromExp _       = throw $ PassError "given expr is not a variable"

argVars :: InvFun -> [(Id,Int)]
argVars (InvFun{..}) = 
  let name n1 = printf "arg_%s_%d" invFunName n1
      ns      = [1..invFunArity]
  in zip (name <$> ns) ns

argVars' :: Id -> [Id] -> [(Id,Int)]
argVars' f as = argVars InvFun{ invFunName   = f
                              , invFunArity  = length as
                              , invFunParams = as
                              }
