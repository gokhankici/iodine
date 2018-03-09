{-# LANGUAGE RecordWildCards #-}

module SMT2FP.SMT.Types where

import           Text.PrettyPrint

type Id = String

data Type    = B                -- boolean
             | I                -- integer
             | A [Type] Type    -- array

data Command = SetLogic Id
             | DeclareFun { dfName :: Id
                          , dfArgs :: [Type]
                          , dfRet  :: Type
                          }
             | DeclareConst { dcName :: Id
                            , dcArg  :: Type
                            }
             | Assert Term
             | CheckSat
             | GetModel

data SortedVar = SortedVar { svSymbol :: Id
                           , svSort   :: Type
                           }

data BinOp  = PLUS | EQU | GE | IMPLIES

data Term = Forall  { sorts :: [SortedVar]
                    , term  :: Term
                    }
          | BinOp   { binop :: BinOp
                    , termL :: Term
                    , termR :: Term
                    }
          | Ands    [Term]
          | App     { fName :: Id
                    , fArgs :: [Id]
                    }
          | Ite     { termC :: Term
                    , termL :: Term
                    , termR :: Term
                    }
          | Select  { arrName    :: Id
                    , arrIndices :: [Id]
                    }
          | Var     Id
          | Number  Int
          | Boolean Bool


class PPrint a where
  toDoc :: a -> Doc

  pprint :: a -> String
  pprint = (renderStyle style{ lineLength     = 150
                             , ribbonsPerLine = 1.2
                             }) . toDoc

instance PPrint Type where
  toDoc B        = text "Bool"
  toDoc I        = text "Int"
  toDoc (A as t) = parens $ text "Array" <+> hsep (toDoc <$> as) <+> toDoc t

instance PPrint Command where
  toDoc (SetLogic l)       = parens $ text "set-logic" <+> text l
  toDoc (DeclareFun{..})   = parens $
                             text dfName <+>
                             parens (hsep (toDoc <$> dfArgs)) <+>
                             toDoc dfRet
  toDoc (DeclareConst{..}) = parens $ text dcName <+> toDoc dcArg
  toDoc (Assert t)         = parens $ text "assert" <+> toDoc t
  toDoc CheckSat           = parens $ text "check-sat"
  toDoc GetModel           = parens $ text "get-model"

instance PPrint SortedVar where
  toDoc (SortedVar{..}) = parens $ text svSymbol <+> toDoc svSort

instance PPrint BinOp where
  toDoc PLUS    = text "+"
  toDoc EQU     = text "="
  toDoc GE      = text ">="
  toDoc IMPLIES = text "=>"

psep :: [Doc] -> Doc
psep = parens . sep

instance PPrint Term where
  toDoc (Forall{..})    = psep [ text "forall"
                               , parens $ sep (toDoc <$> sorts)
                               , toDoc term
                               ]
  toDoc (BinOp{..})     = psep $ (toDoc binop) : (toDoc <$> [termL, termR])
  toDoc (Ite{..})       = psep $ text "ite" : (toDoc <$> [termC, termL, termR])
  toDoc (Var v)         = text v
  toDoc (Number n)      = int n
  toDoc (Boolean True)  = text "true"
  toDoc (Boolean False) = text "false"
  toDoc (Ands ts)       = psep $ (text "and") : (toDoc <$> ts)
  toDoc (Select a is)   = psep $ (text "select") : (text <$> (a:is))
  toDoc (App f ts)      = psep $ (text f) : (text <$> ts)
                               
instance Show Type where
  show = pprint
instance Show Command where
  show = pprint
instance Show SortedVar where
  show = pprint
instance Show BinOp where
  show = pprint
instance Show Term where
  show = pprint
