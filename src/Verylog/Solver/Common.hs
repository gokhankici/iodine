{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Solver.Common where

import Text.Printf
import Control.Lens
import Verylog.Language.Types
import GHC.Generics
import Control.DeepSeq

import Language.Fixpoint.Types (Fixpoint(..), Loc(..), showFix, dummySpan)
import qualified Text.PrettyPrint.HughesPJ as PP

data BinOp = EQU | LE | GE | OR | AND | PLUS | IMPLIES

data InvType = InvInit | InvReTag | InvNext | InvTagEq | InvWF | InvInter Int | InvOther String
            deriving (Generic, Eq, Ord)

instance Fixpoint InvType where
  toFix InvInit      = PP.text "init"
  toFix InvReTag     = PP.text "re-tag"
  toFix InvNext      = PP.text "next"
  toFix InvTagEq     = PP.text "tag eq"
  toFix InvWF        = PP.text "wf"
  toFix (InvInter n) = PP.text "interference w/" PP.<+> PP.int n
  toFix (InvOther s) = PP.text s

instance Show InvType where
  show = showFix

data HornId = HornId Int InvType
            deriving (Generic, Show)

instance Loc HornId where
  srcSpan _ = dummySpan

instance Fixpoint HornId where
  toFix (HornId n t) = PP.parens (toFix t)
                       PP.<+> PP.text "always block id:"
                       PP.<+> PP.int n

instance NFData InvType
instance NFData HornId

data Inv = Horn { hBody :: Expr -- body of the horn clause
                , hHead :: Expr -- head of the horn clause
                , hId   :: HornId
                }

data Expr = BinOp     { bOp   :: BinOp
                      , expL  :: Expr
                      , expR  :: Expr
                      }
          | Ands      [Expr]
          | Ite       { cnd     :: Expr
                      , expThen :: Expr
                      , expElse :: Expr
                      }
          | KV        { kvId   :: Int
                      , kvSubs :: [(Id,Expr)]
                      }
          | Var       Id
          | Boolean   Bool
          | Number    Int
          | UFCheck   { ufArgs  :: [(Expr,Expr)]
                      , ufNames :: (Expr,Expr)
                      , ufFunc  :: Id
                      }

nextPred = "next"
invPred  = "inv"

makeInvPred   :: AlwaysBlock -> String
makeInvPred a = makeInv (a^.aId)

makeInv :: Int -> String
makeInv n = printf "inv%d" n
