{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Transform.Horn where

import Iodine.Language.Types

data Horn a =
       Horn { hornHead :: HornExpr
            , hornBody :: HornExpr
            , hornType :: HornType
            , hornData :: a
            }
       deriving (Show)


data HornBinaryOp = HEquals | HImplies
                  deriving (Show)

data HornType = Init
              | TagReset
              | SourceReset
              | Next
              | TagEqual
              | Interference
              | AssertEqCheck
              | WellFormed
              deriving (Show)

data HornVarType = Tag | Value
                   deriving (Show)

data HornVarRun  = LeftRun | RightRun
                   deriving (Show)

data HornExpr =
  HConstant Id
  | HBool Bool
  | HInt  Int
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
  deriving (Show)
