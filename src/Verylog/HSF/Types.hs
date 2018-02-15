{-# LANGUAGE RecordWildCards #-}

module Verylog.HSF.Types where

import Verylog.Language.Types
import Text.PrettyPrint

data HSFClause = QueryNaming { hsfArgs :: [Id] }
               | Next        { hsfArgs :: [Id]
                             , hsfBody :: HSFExpr
                             }
               | Inv         { hsfArgs :: [Id]
                             , hsfBody :: HSFExpr
                             }

type HSFVar = String

data HSFExpr = BinOp   { hsfOp   :: BinOp
                       , hsfExpL :: HSFExpr
                       , hsfExpR :: HSFExpr
                       }
             | Ands    [HSFExpr]
             | Ors     [HSFExpr]
             | Ite     { hsfCond    :: HSFExpr
                       , hsfExpThen :: HSFExpr
                       , hsfExpElse :: HSFExpr
                       }
             | Var     HSFVar
             | Boolean Bool
             | Number  Int

data BinOp = EQU | LE | GE | OR | AND | PLUS

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

instance PPrint HSFClause where
  toDoc (QueryNaming{..}) = text "query_naming(inv(" <> printArgs hsfArgs <> text "))."
  toDoc (Next{..})        = vcat [ text "next(" <> printArgs hsfArgs <> text ") :=" 
                                 , text "("
                                 , nest 2 $ toDoc hsfBody
                                 , text ")."
                                 ]
  toDoc (Inv{..})         = vcat [ text "inv(" <> printArgs hsfArgs <> text ") :-" 
                                 , text "("
                                 , nest 2 $ toDoc hsfBody
                                 , text ")."
                                 ]

instance PPrint HSFExpr where
  toDoc (Boolean True)  = text "true"
  toDoc (Boolean False) = text "false"
  toDoc (Number n)      = text $ show n
  toDoc (Var x)         = text x
  toDoc (Ands [])       = text "true"
  toDoc (Ors [])        = text "true"
  toDoc (Ands es)       = cat $ punctuate (comma <> space) (toDoc <$> es)
  toDoc (Ors es)        = cat $ punctuate (semi <> space)  (parens . toDoc <$> es)
  toDoc (Ite{..})       = text "ite("
                          <> toDoc hsfCond <> comma
                          <+> toDoc hsfExpThen <> comma
                          <+> toDoc hsfExpElse
                          <> rparen
  toDoc (BinOp{..})     = case hsfOp of
                            EQU  -> toDoc hsfExpL <+> equals <+> toDoc hsfExpR
                            LE   -> toDoc hsfExpL <+> text "=<" <+> toDoc hsfExpR
                            GE   -> toDoc hsfExpL <+> text ">=" <+> toDoc hsfExpR
                            PLUS -> toDoc hsfExpL <+> text "+"  <+> toDoc hsfExpR
                            AND  -> vcat [ toDoc hsfExpL <> comma
                                         , toDoc hsfExpR
                                         ]
                            OR   -> vcat [ lparen <> text "   " <> toDoc hsfExpL
                                         , semi   <> text "   " <> toDoc hsfExpR
                                         , rparen
                                         ]
