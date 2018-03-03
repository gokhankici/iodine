module Verylog.Solver.Common where

import Text.Printf
import Control.Lens
import Verylog.Language.Types

data BinOp = EQU | LE | GE | OR | AND | PLUS | IMPLIES
data UnOp  = NOT

data Inv = Inv  { invId   :: Int
                , invArgs :: [Id]
                , invBody :: Expr
                }
         | Prop { propHead :: Expr
                , propBody :: Expr
                }

data Expr = BinOp     { bOp   :: BinOp
                      , expL  :: Expr
                      , expR  :: Expr
                      }
          | UnOp      { uOp  :: UnOp
                      , exp  :: Expr
                      }
          | Ands      [Expr]
          | Ors       [Expr]
          | Ite       { cnd     :: Expr
                      , expThen :: Expr
                      , expElse :: Expr
                      }
          | Structure Id [Id]
          | Var       Id
          | Boolean   Bool
          | Number    Int
          | UFCheck   { ufArgs  :: [(Expr,Expr)]
                      , ufNames :: (Expr,Expr)
                      }

nextPred = "next"
invPred  = "inv"

makeInvPred   :: AlwaysBlock -> String
makeInvPred a = printf "inv%d" (a^.aId)
