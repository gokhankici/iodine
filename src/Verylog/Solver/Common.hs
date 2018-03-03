module Verylog.Solver.Common where

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
