{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.SMT.Types where

import           Control.Monad.State.Lazy
import           Text.PrettyPrint
import           Text.Printf
import qualified Data.HashSet               as S

import           Verylog.Language.Types
import           Verylog.Solver.Common

type SMTVar = String

data InvFun = InvFun { invFunName  :: Id
                     , invFunArity :: Int
                     }

data UFConst = UFConst { ufConstName  :: Id
                       , ufConstArity :: Int
                       }

forallArgs   :: Expr -> Doc
forallArgs e = psep [text "forall" , args, toDoc e]
  where
    args = let f a = parens $ text a <+> text "Int"
           in psep (f <$> allVars e)

instance PPrint Inv where
  toDoc (Inv{..})  = parens $ text "assert" <+> chk
    where
      chk     = forallArgs (BinOp IMPLIES invBody Structure{ propName   = invName
                                                           , propArgs   = invArgs
                                                           , propParams = invParams
                                                           })
      invName = printf "%s%d" invPred invId

  toDoc (Prop{..}) = parens $ text "assert" <+> chk
    where
      chk = forallArgs (BinOp IMPLIES propL propR)

instance PPrint Expr where
  toDoc (Boolean True)   = text "true"
  toDoc (Boolean False)  = text "false"
  toDoc (Number n)       = text $ show n
  toDoc (Var x)          = text x
  toDoc (Ands [])        = text "true"
  toDoc (Ands as)        = psep (text "and" : (toDoc <$> as))
  toDoc (Ite{..})        = parens $ text "ite" <+> cat [ ptoDoc cnd
                                                       , ptoDoc expThen
                                                       , ptoDoc expElse
                                                       ]
  toDoc (Structure{..})  = parens $ hsep (text <$> (propName:propArgs))
  toDoc (BinOp{..})      = let op = case bOp of
                                      IMPLIES -> "=>"
                                      EQU     -> "="
                                      LE      -> "<="
                                      GE      -> ">="
                                      PLUS    -> "+"
                                      AND     -> "and"
                                      OR      -> "or"
                           in psep [ text op
                                   , toDoc expL
                                   , toDoc expR
                                   ]
  toDoc (UFCheck{..}) = psep [ text "and"
                             , (sel (fst ufNames) (fst <$> ufArgs))
                             , (sel (snd ufNames) (snd <$> ufArgs))
                             ]
    where
      sel f args = psep [ text "="
                        , toDoc f
                        , psep (text "select" : text ufFunc : (toDoc <$> args))
                        ]

instance PPrint InvFun where
  toDoc (InvFun{..}) = parens $ hsep [ text "declare-fun"
                                     , text invFunName
                                     , parens $ hsep (replicate invFunArity (text "Int"))
                                     , text "Bool"
                                     ]

instance PPrint UFConst where
  toDoc (UFConst{..}) = parens $ hsep [ text "declare-const"
                                      , text ufConstName
                                      , parens $ hsep $
                                        text "Array" : (replicate (ufConstArity+1) (text "Int"))
                                      ]

ptoDoc :: PPrint a => a -> Doc
ptoDoc = parens . toDoc

psep :: [Doc] -> Doc
psep = parens . sep

pcat :: [Doc] -> Doc
pcat = parens . cat

type S = State (S.HashSet Id)

-- TODO : this is probably not quite right
--------------------------------------------------------------------------------
allVars :: Expr -> [Id]
--------------------------------------------------------------------------------
allVars s = evalState comp S.empty
  where
    comp = f s >> get >>= return . S.toList

    f                  :: Expr -> S ()
    f (BinOp{..})      = sequence_ (f <$> [expL, expR])
    f (Ands es)        = sequence_ (f <$> es)
    f (Ite{..})        = sequence_ (f <$> [cnd, expThen, expElse])
    f (Structure{..})  = modify (S.union (S.fromList propArgs))
    f (Var v)          = modify (S.insert v)
    f (Boolean _)      = return ()
    f (Number _)       = return ()
    f (UFCheck{..})    = sequence_ (f <$> as ++ t2l ufNames)
      where
        as        = concatMap t2l ufArgs
        t2l (x,y) = [x,y]

instance Show Inv where
  show = pprint
instance Show InvFun where
  show = pprint
instance Show UFConst where
  show = pprint
