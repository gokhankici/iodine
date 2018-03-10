module Verylog.Solver.FP.FQ ( toFqFormat ) where

import Verylog.Solver.FP.Types (FPSt(..))

-- import Language.Fixpoint.Solver
import Language.Fixpoint.Types.Constraints

-- FI	 
--   cm       :: !(HashMap SubcId (c a))     cst id |-> Horn Constraint
--   ws       :: !(HashMap KVar (WfC a))     Kvar |-> WfC defining its scope/args
--   bs       :: !BindEnv                    Bind |-> (Symbol, SortedReft)
--   gLits    :: !(SEnv Sort)                Global Constant symbols
--   dLits    :: !(SEnv Sort)                Distinct Constant symbols
--   kuts     :: !Kuts                       Set of KVars *not* to eliminate
--   quals    :: ![Qualifier]                Abstract domain
--   bindInfo :: !(HashMap BindId a)         Metadata about binders
--   ddecls   :: ![DataDecl]                 User-defined data declarations
--   hoInfo   :: !HOInfo                     Higher Order info
--   asserts  :: ![Triggered Expr]
--   ae       :: AxiomEnv	


-- fi :: [SubC a] ->
--       [WfC a] ->
--       BindEnv ->
--       SEnv Sort ->
--       SEnv Sort ->
--       Kuts ->
--       [Qualifier] ->
--       HashMap BindId a ->
--       Bool ->
--       Bool ->
--       [Triggered Expr] ->
--       AxiomEnv ->
--       [DataDecl] ->
--       GInfo SubC a

toFqFormat :: FPSt -> GInfo SubC ()
toFqFormat fpst =
  let cns         = makeConstraints fpst
      wfs         = undefined
      binders     = undefined
      gConsts     = undefined
      dConsts     = undefined
      cuts        = undefined
      qualifiers  = undefined
      bindMds     = undefined
      highOrBinds = undefined
      highOrQuals = undefined
      assrts      = undefined
      axiomEnv    = undefined
      dataDecls   = undefined
  in  fi cns wfs binders gConsts dConsts cuts qualifiers bindMds highOrBinds highOrQuals assrts axiomEnv dataDecls 

makeConstraints :: FPSt -> [SubC ()]
makeConstraints _fpst = mc <$> undefined
  where
    mc = undefined
