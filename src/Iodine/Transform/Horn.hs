{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Iodine.Transform.Horn where

import GHC.Generics
import           Control.DeepSeq
import           Iodine.Language.Types
import qualified Language.Fixpoint.Types       as FT
import qualified Text.PrettyPrint.HughesPJ     as PP

data Horn a =
       Horn { hornHead :: HornExpr
            , hornBody :: HornExpr
            , hornType :: HornType
            , hornData :: a
            }
       deriving (Show, Functor)


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
              deriving (Show, Generic)

data HornVarType = Tag | Value
                   deriving (Show)

data HornVarRun  = LeftRun | RightRun
                   deriving (Show)

data HornExpr =
  HConstant Id
  | HBool Bool
  | HInt  Int
  | HVar { hVarName   :: Id
         , hVarModule :: Id
         , hVarIndex  :: Int
         , hVarType   :: HornVarType
         , hVarRun    :: HornVarRun
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


instance FT.Fixpoint HornType where
       toFix Init          = PP.text "init"
       toFix TagReset      = PP.text "tag-reset"
       toFix SourceReset   = PP.text "source-reset"
       toFix Next          = PP.text "next"
       toFix TagEqual      = PP.text "tag-equal"
       toFix Interference  = PP.text "interference"
       toFix AssertEqCheck = PP.text "assert-eq"
       toFix WellFormed    = PP.text "wellformed"

instance NFData HornType
