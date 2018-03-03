{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.SMT.Types where

import Text.PrettyPrint
import Text.Printf
import Verylog.Language.Types

import Verylog.Solver.Common

type SMTVar = String

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

forallArgs      :: [Id] -> Expr -> Doc
forallArgs as e = psep [text "forall" , args, toDoc e]
  where
    args = let f a = parens $ text a <+> text "Int"
           in psep (f <$> as)

instance PPrint Inv where
  toDoc (Inv{..})  = parens $ text "assert" <+> chk
    where
      chk     = forallArgs invArgs (BinOp IMPLIES invBody (Structure invName invArgs))
      invName = printf "%s%d" invPred invId

  toDoc (Prop{..}) = parens $ text "assert" <+> chk
    where
      chk = forallArgs propArgs (BinOp IMPLIES propL propR)

instance PPrint Expr where
  toDoc (Boolean True)   = text "true"
  toDoc (Boolean False)  = text "false"
  toDoc (Number n)       = text $ show n
  toDoc (Var x)          = text x
  toDoc (Ands [])        = text "true"
  toDoc (Ands as)        = psep (text "and" : (toDoc <$> as))
  toDoc (Ite{..})        = psep [ text "ite" 
                                , toDoc cnd
                                , toDoc expThen
                                , toDoc expElse
                                ]
  toDoc (Structure f as) = parens $ hsep (text <$> (f:as))
  toDoc (UnOp{..})       = case uOp of
                             NOT -> text "!" <> toDoc exp
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
        

ptoDoc :: PPrint a => a -> Doc
ptoDoc = parens . toDoc

psep :: [Doc] -> Doc
psep = parens . sep

instance Show Expr where
  show = pprint

instance Show Inv where
  show = pprint

