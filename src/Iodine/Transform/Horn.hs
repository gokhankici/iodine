{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Transform.Horn where

import Iodine.Language.Types

data Horn a = Horn { hornHead :: HornExpr
                   , hornBody :: HornExpr
                   , hornType :: HornType
                   , hornData :: a
                   }

data HornBinaryOp = HEquals | HImplies

data HornType = Init
              | TagReset
              | SourceReset
              | Next
              | TagEqual
              | Interference
              | WellFormed

data HornVarType = Tag | Value

data HornVarRun  = LeftRun | RightRun

data HornExpr =
  HBool Bool
  | HVar { hVarName  :: Id
         , hVarIndex :: Int
         , hVarType  :: HornVarType
         , hVarRun   :: HornVarRun
         }
  | HAnd { hAppArgs :: L HornExpr }
  | HOr  { hAppArgs :: L HornExpr }
  | HBinary { hBinaryOp  :: HornBinaryOp
            , hBinaryLhs :: HornExpr
            , hBinaryRhs :: HornExpr
            }
  | HApp { hAppArgs :: L HornExpr }
  | HNot { hNotArg :: HornExpr }
  | KVar { hKVarId   :: Int
         , hKVarSubs :: L (HornExpr, HornExpr)
         }
