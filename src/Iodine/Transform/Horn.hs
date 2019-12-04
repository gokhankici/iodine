{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Iodine.Transform.Horn where

import           Iodine.Types

import           GHC.Generics
import           Control.DeepSeq
import qualified Data.Text as T
import           Data.Foldable
import qualified Language.Fixpoint.Types as FT
import qualified Text.PrettyPrint as PP

data Horn a =
       Horn { hornHead   :: HornExpr
            , hornBody   :: HornExpr
            , hornType   :: HornType
            , hornStmtId :: Int
            , hornData   :: a
            }
       deriving (Show, Functor)


data HornBinaryOp = HEquals | HNotEquals | HImplies | HIff

data HornType = Init
              | TagReset
              | SourceReset
              | Next
              | TagEqual
              | Interference
              | AssertEqCheck
              | WellFormed
              deriving (Eq, Show, Generic)

data HornVarType = Tag | Value
                   deriving (Eq, Show)

data HornVarRun  = LeftRun | RightRun
                   deriving (Eq, Show)

data HornAppReturnType = HornInt | HornBool

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
  | HApp { hAppFun  :: Id                -- | a unique function name
         , hAppRet  :: HornAppReturnType -- | function return type
         , hAppArgs :: L HornExpr        -- | function arguments
         }
  | HNot { hNotArg :: HornExpr }
  | KVar { hKVarId   :: Int
         , hKVarSubs :: L (HornExpr, HornExpr)
         }

mkEqual :: (HornExpr, HornExpr) -> HornExpr
mkEqual (e1, e2) = HBinary op e1 e2
  where op = if isBoolean e1 || isBoolean e2 then HIff else HEquals


isBoolean :: HornExpr -> Bool
isBoolean (HBool _) = True
isBoolean HVar{..}  = hVarType == Tag
isBoolean HAnd{..}  = True
isBoolean HOr{..}   = True
isBoolean HNot{..}  = True
isBoolean _         = False


instance Show HornBinaryOp where
  show HEquals    = "="
  show HNotEquals = "!="
  show HImplies   = "=>"
  show HIff       = "<=>"

instance Show HornExpr where
  show = PP.render . go
    where
      text = PP.text . T.unpack
      goArgs = PP.cat . PP.punctuate (PP.comma PP.<+> PP.empty) . toList . fmap go
      goL = PP.brackets . goArgs
      go = \case
        HConstant c -> text c
        HBool b     -> PP.text $ show b
        HInt n      -> PP.int n
        HVar{..}    ->
          let prefix = case (hVarType, hVarRun) of
                         (Tag, LeftRun)    -> "TL"
                         (Tag, RightRun)   -> "TR"
                         (Value, LeftRun)  -> "VL"
                         (Value, RightRun) -> "VR"
          in PP.hcat $ PP.punctuate (PP.char '.')
             [PP.text prefix , text hVarModule , text hVarName , PP.int hVarIndex]
        HAnd es -> PP.text "&&" PP.<+> goL es
        HOr es -> PP.text "||" PP.<+> goL es
        HBinary{..} -> PP.hsep [go hBinaryLhs, PP.text (show hBinaryOp), go hBinaryRhs]
        HApp{..} -> text hAppFun PP.<> PP.parens (goArgs hAppArgs)
        HNot e -> PP.char '!' PP.<+> go e
        KVar{..} ->
          let args =
                toList $
                (\(v,e) -> PP.brackets $ PP.hsep [ go v , PP.text ":=" , go e]) <$>
                hKVarSubs
          in PP.hcat [ PP.char '$'
                     , PP.text "inv"
                     , PP.int hKVarId
                     , PP.hcat args
                     ]


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

pattern HVar0 :: Id -> Id -> HornVarType -> HornVarRun -> HornExpr
pattern HVar0 v m t r =
  HVar { hVarName   = v
       , hVarModule = m
       , hVarIndex  = 0
       , hVarType   = t
       , hVarRun    = r
       }

pattern HVarVL0 :: Id -> Id -> HornExpr
pattern HVarVL0 v m = HVar0 v m Value LeftRun

pattern HVarVR0 :: Id -> Id -> HornExpr
pattern HVarVR0 v m = HVar0 v m Value RightRun

pattern HVarTL0 :: Id -> Id -> HornExpr
pattern HVarTL0 v m = HVar0 v m Tag LeftRun

pattern HVarTR0 :: Id -> Id -> HornExpr
pattern HVarTR0 v m = HVar0 v m Tag RightRun

pattern HVarT0 :: Id -> Id -> HornVarRun -> HornExpr
pattern HVarT0 v m r = HVar0 v m Tag r
