module Verylog.Types where

import Verylog.Language.Types
import Verylog.Solver.FP.Types

import Data.Sequence

type ABS a        = Seq (AlwaysBlockA a)
type States       = (St, AnnotSt)

type Qualifiers a = [FPQualifierA a]

type ParseInput   = (FilePath, String)
type ParseOutput  = (States, Qualifiers Id)

type Intermediary      = IntermediaryA Id
type IntermediaryA a   = (ABS a, IntermediaryA_2 a)
type IntermediaryA_2 a = (AnnotStA a, Qualifiers a)
