{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Iodine.Solver.Common where

import Iodine.Language.Types ( Id
                              , AlwaysBlockA , aId
                              )

import Language.Fixpoint.Types ( Fixpoint(..)
                               , Loc(..)
                               , showFix
                               , dummySpan
                               )

import           Control.DeepSeq
import           Control.Lens
import qualified Data.IntMap.Strict        as IM
import           GHC.Generics
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf
import           Data.Sequence             as SQ

-- -----------------------------------------------------------------------------
-- data types
-- -----------------------------------------------------------------------------

data BinOp = EQU | LE | GE | OR | AND | PLUS | IMPLIES | IFF
           deriving (Show, Eq, Generic, Ord)

data InvType = InvInit     Int
             | InvReTag    Int
             | InvSrcReset Int
             | InvNext     Int
             | InvTagEq    Int
             | InvWF       Int
             | InvInter    Int
             | InvOther    String
            deriving (Generic, Eq, Ord)

data HornId = HornId Int InvType
            deriving (Generic, Show)

data Inv = Horn { hBody :: Expr -- body of the horn clause
                , hHead :: Expr -- head of the horn clause, must be a kvar
                , hId   :: HornId
                }
           deriving (Show, Generic)

type Constraints = IM.IntMap (SQ.Seq Inv)

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
                      , kvSubs :: SQ.Seq (Id,Expr)
                      }
          | Var       Id
          | Boolean   Bool
          | Number    Int
          | UFCheck   { ufArgs  :: [(Expr,Expr)]
                      , ufNames :: (Expr,Expr)
                      , ufFunc  :: Id
                      }
          deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------------

instance Show InvType where
  show = showFix

instance Fixpoint InvType where
  toFix (InvInit n)     = PP.text "init of block"      PP.<+> PP.int n
  toFix (InvReTag n)    = PP.text "re-tag of block"    PP.<+> PP.int n
  toFix (InvSrcReset n) = PP.text "src-reset of block" PP.<+> PP.int n
  toFix (InvNext n)     = PP.text "next of block"      PP.<+> PP.int n
  toFix (InvTagEq n)    = PP.text "tag eq of block"    PP.<+> PP.int n
  toFix (InvWF n)       = PP.text "wf of block"        PP.<+> PP.int n
  toFix (InvInter n)    = PP.text "interference with"  PP.<+> PP.int n
  toFix (InvOther s)    = PP.text s

instance Loc HornId where
  srcSpan _ = dummySpan

instance Fixpoint HornId where
  toFix (HornId n t) = PP.parens (toFix t)
                       PP.<+> PP.text "always block id:"
                       PP.<+> PP.int n

instance NFData InvType
instance NFData HornId
instance NFData BinOp
instance NFData Expr
instance NFData Inv

-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

nextPred :: String
nextPred = "next"

invPred :: String
invPred  = "inv"

makeInvPred   :: AlwaysBlockA a -> String
makeInvPred a = makeInv (a^.aId)

makeInv :: Int -> String
makeInv = printf "inv%d"
