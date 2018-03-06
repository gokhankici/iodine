{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.HSF.Types where

import Verylog.Language.Types
import Text.PrettyPrint

import Verylog.Solver.Common

data QueryNaming = QueryNaming { hsfId   :: Int
                               , hsfArgs :: [Id]
                               }

type HSFVar = String

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

instance PPrint QueryNaming where
  toDoc (QueryNaming{..}) = text "query_naming("
                            <> text invPred <> int hsfId
                            <> lparen <> printArgs hsfArgs <> text "))."

instance PPrint Inv where
  toDoc (Inv{..})  = text invPred <> int invId <> lparen <> printArgs invArgs <> text ") :-" 
                     $+$ text "("
                     $+$ nest 3 (toDoc invBody)
                     $+$ text ")."
  toDoc (Prop{..}) = toDoc propR <+> text ":-" 
                     $+$ nest 3 (toDoc propL) <> text "."
                            

instance PPrint Expr where
  toDoc (Boolean True)   = text "true"
  toDoc (Boolean False)  = text "false"
  toDoc (Number n)       = text $ show n
  toDoc (Var x)          = text x
  toDoc (Ands [])        = text "true"
  toDoc (Ands es)        = cat $ punctuate (comma <> space) (toDoc <$> es)
  toDoc (Structure f as) = text f <> lparen <> printArgs as <> rparen
  toDoc (Ite{..})        = text "ite" <> pcat [ ptoDoc cnd     <> comma
                                              , ptoDoc expThen <> comma
                                              , ptoDoc expElse
                                              ]
  toDoc (BinOp{..})      = case bOp of
                             IMPLIES -> lparen
                                        <> ptoDoc expL <+> text "->" <+> ptoDoc expR
                                        <> semi <+> text "true"
                                        <> rparen
                             EQU     -> toDoc expL <+> equals    <+> toDoc expR
                             LE      -> toDoc expL <+> text "=<" <+> toDoc expR
                             GE      -> toDoc expL <+> text ">=" <+> toDoc expR
                             PLUS    -> toDoc expL <+> text "+"  <+> toDoc expR
                             AND     -> cat [ toDoc expL <> comma
                                            , toDoc expR
                                            ]
                             OR      -> vcat [ lparen <> text "   " <> toDoc expL
                                             , semi   <> text "   " <> toDoc expR
                                             , rparen
                                             ]
  toDoc (UFCheck{..}) = doc
    where
      doc        = lparen
                   <> cat [ nest 1 $ text "\\+" <> ptoDoc antecedent
                          , semi <+> toDoc consequent
                          ]
                   <> rparen
      antecedent = Ands $ uncurry (BinOp EQU) <$> ufArgs
      consequent = uncurry (BinOp EQU) ufNames
      

ptoDoc :: PPrint a => a -> Doc
ptoDoc = parens . toDoc

pcat :: [Doc] -> Doc
pcat = parens . cat

instance Show QueryNaming where
  show = pprint
instance Show Inv where
  show = pprint
