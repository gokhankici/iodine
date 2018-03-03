{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.SMT.Types where

import Verylog.Language.Types
import Text.PrettyPrint
import Text.Printf
import Control.Lens

import Verylog.Solver.Common

type SMTVar = String

printArgs as = hcat $ punctuate (comma <> space) (text <$> as)

nextPred = "next"
invPred  = "inv"

makeInvPred   :: AlwaysBlock -> String
makeInvPred a = printf "inv%d" (a^.aId)

instance PPrint Inv where
  toDoc (Inv{..})  = undefined
  toDoc (Prop{..}) = undefined

instance PPrint Expr where
  toDoc = undefined
                            
ptoDoc :: PPrint a => a -> Doc
ptoDoc = parens . toDoc

instance Show Expr where
  show = pprint

instance Show Inv where
  show = pprint

