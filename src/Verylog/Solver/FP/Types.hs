{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Verylog.Solver.FP.Types where

import           Verylog.Language.Types hiding (St)
import           Verylog.Solver.Common
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Data.Sequence
import qualified Data.HashMap.Strict        as M
import           GHC.Generics hiding (to)
import qualified Language.Fixpoint.Types    as FQ

type FPQualifier = FPQualifierA Id
data FPQualifierA a =
    QualifImp    { qualifLhs  :: a
                 , qualifRhss :: [a]
                 }
  | QualifIff    { qualifLhs  :: a
                 , qualifRhss :: [a]
                 }
  | QualifPairs  { qualifEqs    :: [a] }
  | QualifAssume { qualifAssume :: [a] }
  deriving (Generic, Show)

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     , bindType :: FQ.Sort
                     , bindRef  :: FQ.Expr
                     }
            deriving (Generic)

data InvFun = InvFun { invFunName   :: Id
                     , invFunArity  :: Int
                     , invFunParams :: [Id]
                     }

data UFConst = UFConst { ufConstName  :: Id
                       , ufConstArity :: Int
                       }

type BindMap = M.HashMap Id FQBind

type FPSt = FPStA Id
data FPStA a =
  FPSt { _fpConstraints :: Constraints
       , _fpABs         :: Seq (AlwaysBlockA a)
       , _fpBinds       :: BindMap
       , _fpQualifiers  :: [FPQualifier]
       , _fpAnnotations :: AnnotStA a
       }
  deriving (Generic)

makeLenses ''FPStA

instance PPrint a => Show (FPStA a) where
  show fpst = show (fpst ^. fpABs)

instance NFData FQBind
instance NFData a => NFData (FPStA a)
instance NFData a => NFData (FPQualifierA a)

idFromExp :: Expr -> Id
idFromExp (Var v) = v
idFromExp _       = throw $ PassError "given expr is not a variable"

qualifVars :: FPQualifier -> [Id]
qualifVars (QualifImp l rs)  = l:rs
qualifVars (QualifIff l rs)  = l:rs
qualifVars (QualifPairs vs)  = vs
qualifVars (QualifAssume vs) = vs

