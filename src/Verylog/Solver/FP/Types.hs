{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Solver.FP.Types ( FQBind(..)
                               , InvFun(..)
                               , UFConst(..)
                               , BindMap
                               , UFMap
                               , FPQualifier(..)
                               , qualifVars

                               , FPSt(..)
                               , fpConstraints
                               , fpABs
                               , fpBinds
                               , fpUFs
                               , fpQualifiers
                               , fpSources

                               , idFromExp
                               ) where

import           Verylog.Language.Types hiding (St, ufs)
import           Verylog.Solver.Common

import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import qualified Data.HashMap.Strict        as M
import           GHC.Generics hiding (to)
import qualified Language.Fixpoint.Types    as FQ

data FPQualifier = QualifImp    { qualifLhs  :: !Id
                                , qualifRhss :: ![Id]
                                }
                 | QualifIff    { qualifLhs  :: !Id
                                , qualifRhss :: ![Id]
                                }
                 | QualifPairs  { qualifEqs :: ![Id] }
                 | QualifAssume { qualifAssume :: ![Id] }
                 deriving (Generic, Show)

data FQBind = FQBind { bindId   :: ! Int
                     , bindName :: ! Id
                     , bindType :: ! FQ.Sort
                     , bindRef  :: ! FQ.Expr
                     }
            deriving (Generic)

data InvFun = InvFun { invFunName   :: ! Id
                     , invFunArity  :: ! Int
                     , invFunParams :: ! [Id]
                     }

data UFConst = UFConst { ufConstName  :: ! Id
                       , ufConstArity :: ! Int
                       }

type BindMap = M.HashMap Id FQBind

data FPSt = FPSt { _fpConstraints :: ! [Inv]
                 , _fpABs         :: ! [AlwaysBlock]
                 , _fpBinds       :: ! BindMap
                 , _fpUFs         :: ! UFMap
                 , _fpQualifiers  :: ! [FPQualifier]
                 , _fpSources     :: ! [Id]
                 }
            deriving (Generic)

makeLenses ''FPSt

instance Show FPSt where
  show fpst = show (fpst ^. fpABs)

instance NFData FQBind
instance NFData FPSt
instance NFData FPQualifier

idFromExp :: Expr -> Id
idFromExp (Var v) = v
idFromExp _       = throw $ PassError "given expr is not a variable"

qualifVars :: FPQualifier -> [Id]
qualifVars (QualifImp l rs)  = l:rs
qualifVars (QualifIff l rs)  = l:rs
qualifVars (QualifPairs vs)  = vs
qualifVars (QualifAssume vs) = vs
