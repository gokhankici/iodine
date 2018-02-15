{-# LANGUAGE RecordWildCards #-}

module Verylog.HSF.Types where

import Verylog.Language.Types
import Text.PrettyPrint

data HSFClause = QueryNaming { hsfArgs :: [Id] }
               | Next        { hsfArgs :: [HSFVar]
                             , hsfBody :: HSFExpr
                             }
               | Inv         { hsfArgs :: [HSFVar]
                             , hsfBody :: HSFExpr
                             }
               | Prop        { hsfHead :: HSFExpr
                             , hsfBody :: HSFExpr
                             }

type HSFVar = String

data HSFExpr = BinOp     { hsfOp   :: BinOp
                         , hsfExpL :: HSFExpr
                         , hsfExpR :: HSFExpr
                         }
             | Ands      [HSFExpr]
             | Ors       [HSFExpr]
             | Ite       { hsfCond    :: HSFExpr
                         , hsfExpThen :: HSFExpr
                         , hsfExpElse :: HSFExpr
                         }
             | Structure Id [Id]
             | Var       HSFVar
             | Boolean   Bool
             | Number    Int

data BinOp = EQU | LE | GE | OR | AND | PLUS

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

instance PPrint HSFClause where
  toDoc (QueryNaming{..}) = text "query_naming(inv(" <> printArgs hsfArgs <> text "))."
  toDoc (Next{..})        = text "next(" <> printArgs hsfArgs <> text ") :=" 
                            $+$ text "("
                            $+$ nest 3 (toDoc hsfBody)
                            $+$ text ")."
  toDoc (Inv{..})         = text "inv(" <> printArgs hsfArgs <> text ") :-" 
                            $+$ text "("
                            $+$ nest 3 (toDoc hsfBody)
                            $+$ text ")."
  toDoc (Prop{..})        = toDoc hsfHead <+> text ":-" 
                            $+$ nest 3 (toDoc hsfBody) <> text "."
                            

instance PPrint HSFExpr where
  toDoc (Boolean True)   = text "true"
  toDoc (Boolean False)  = text "false"
  toDoc (Number n)       = text $ show n
  toDoc (Var x)          = text x
  toDoc (Ands [])        = text "true"
  toDoc (Ors [])         = text "true"
  toDoc (Ands es)        = cat $ punctuate (comma <> space) (toDoc <$> es)
  toDoc (Ors es)         = cat $ punctuate (semi <> space)  (parens . toDoc <$> es)
  toDoc (Structure f as) = text f <> lparen <> (hcat $ punctuate (comma <> space) (text <$> as)) <> rparen
  toDoc (Ite{..})        = text "ite("
                           <> toDoc hsfCond <> comma
                           <+> toDoc hsfExpThen <> comma
                           <+> toDoc hsfExpElse
                           <> rparen
  toDoc (BinOp{..})      = case hsfOp of
                             EQU  -> toDoc hsfExpL <+> equals <+> toDoc hsfExpR
                             LE   -> toDoc hsfExpL <+> text "=<" <+> toDoc hsfExpR
                             GE   -> toDoc hsfExpL <+> text ">=" <+> toDoc hsfExpR
                             PLUS -> toDoc hsfExpL <+> text "+"  <+> toDoc hsfExpR
                             AND  -> cat [ toDoc hsfExpL <> comma
                                         , toDoc hsfExpR
                                         ]
                             OR   -> cat [ lparen <> text "   " <> toDoc hsfExpL
                                         , semi   <> text "   " <> toDoc hsfExpR
                                         , rparen
                                         ]
