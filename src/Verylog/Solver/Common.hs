{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Solver.Common where

import Text.Printf
import Control.Lens
import Verylog.Language.Types
import GHC.Generics
import Control.DeepSeq

data BinOp = EQU | LE | GE | OR | AND | PLUS | IMPLIES

data HornId = SingleBlock       Int
            | InterferenceBlock Int Int
            deriving (Generic, Show)

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
