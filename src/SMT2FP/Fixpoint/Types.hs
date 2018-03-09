{-# LANGUAGE RecordWildCards #-}
module SMT2FP.Fixpoint.Types where

import SMT2FP.SMT.Types
import Control.Exception
import Data.Typeable
import Text.PrettyPrint

data FQInv = FQInv { invName  :: Id
                   , invArity :: Int
                   }

data FQUF = FQUF { ufId    :: Int
                 , ufName  :: Id
                 , ufArity :: Int
                 }

data FQBinOp = FQIMPLIES -- ==>
             | FQEQU     -- ==
             | FQPLUS    -- +
             | FQGE      -- >=

data FQExpr = FQBinOp   { fqExpOp :: FQBinOp
                        , fqExpL :: FQExpr
                        , fqExpR :: FQExpr
                        }
            | FQNot     FQExpr
            | FQAnds    [FQExpr]
            | FQInvCall { callInv  :: FQInv 
                        , callArgs :: [Id]
                        }
            | FQUFCall  { callUF    :: FQUF
                        , callArgs  :: [Id]
                        }
            | FQVar     Id
            | FQNumber  Int
            | FQBoolean Bool

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     , bindExpr :: FQExpr
                     }

data FQConstraint = FQConstraint { constraintId     :: Int 
                                 , constraintBinds :: [FQBind]
                                 , constraintLhs   :: FQExpr
                                 , constraintRhs   :: FQExpr
                                 }

data FQWFConstraint = FQWFConstraint { wfInv :: FQInv
                                     }

data PassError = PassError !String
               deriving (Show, Typeable)

instance Exception PassError

typeDef :: (PPrint a) => a -> Doc
typeDef t = 
    braces (text "v" <> colon <+> text "int" <+> text "|" <+> toDoc t)

instance PPrint FQBind where
  toDoc (FQBind{..}) =
    text "bind" <+> int bindId <+> text bindName <+> colon <+> typeDef bindExpr

instance PPrint FQExpr where
  toDoc (FQBinOp   {..}) = parens $ sep [ toDoc fqExpL
                                        , toDoc fqExpOp
                                        , toDoc fqExpR
                                        ]
  toDoc (FQAnds    es)   = brackets $ sep $ punctuate semi (toDoc <$> es)
  toDoc (FQInvCall {..}) = text "<<<fqinvcall>>>" -- TODO
  toDoc (FQUFCall  {..}) = text "<<<fqufcall>>>"  -- TODO
  toDoc (FQVar     v)    = text v
  toDoc (FQNumber  n)    = int n
  toDoc (FQNot     e)    = text "!" <> parens (toDoc e)
  toDoc (FQBoolean b)    = if b then text "true" else text "false"
    
instance PPrint FQBinOp where
  toDoc FQIMPLIES = text "==>"
  toDoc FQEQU     = text "="
  toDoc FQPLUS    = text "+"
  toDoc FQGE      = text ">="

instance PPrint FQConstraint where
  toDoc (FQConstraint{..}) = vcat [ text "constraint:"
                                  , nest 2 body
                                  ]
    where
      body = vcat [ text "env" <+> brackets (hsep $ punctuate semi ((int . bindId) <$> constraintBinds))
                  , text "lhs" <+> typeDef constraintLhs
                  , text "rhs" <+> typeDef constraintRhs
                  , text "id" <+> int constraintId <+> text "tag []"
                  ]

instance PPrint FQWFConstraint where
  toDoc (FQWFConstraint{..}) = vcat [ text "wf:"
                                    , nest 2 body
                                    ]
    where
      body = vcat [ text "env" <+> brackets empty
                  , text "reft" <+> text "<<<reft>>>" -- TODO
                  ]

instance PPrint FQUF where
  toDoc (FQUF{..}) =
    text "bind" <+> int ufId <+> text ufName <+> colon <+>
    braces (text "v" <+> colon <+> text "<<<map>>>" <+> text "|" <+> text "true")

instance Show FQBind where
  show = pprint
instance Show FQExpr where
  show = pprint
instance Show FQConstraint where
  show = pprint
instance Show FQWFConstraint where
  show = pprint
