{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.Query
  ( constructQuery
  , FInfo
  , QueryError(..)
  )
where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence                 as SQ
import qualified Data.Text                     as T
import           GHC.Generics            hiding ( moduleName )
import qualified Language.Fixpoint.Types       as FT
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace
import           Polysemy.State
import qualified Polysemy.Error                as PE
import qualified Text.PrettyPrint.HughesPJ     as PP

import           Iodine.Language.Annotation
import           Iodine.Language.Types
import           Iodine.Transform.Horn

-- -----------------------------------------------------------------------------
-- solver state
-- -----------------------------------------------------------------------------
data HornClauseId = HornClauseId { hcStmtId :: Int, hcType :: HornType }
                  deriving (Show, Generic)

data St = St { _hornConstraints           :: HM.HashMap Integer (FT.SubC HornClauseId)
             , _wellFormednessConstraints :: HM.HashMap FT.KVar (FT.WfC HornClauseId)
             , _bindEnvironment           :: FT.BindEnv
             , _globalConstantLiterals    :: FT.SEnv FT.Sort
             , _qualifiers                :: SQ.Seq FT.Qualifier

             , _constraintCounter         :: Integer
             , _uninterpretedFuncCounter  :: Int
             , _invBindMap                :: HM.HashMap Id FT.BindId
             }

makeLenses ''St

-- -----------------------------------------------------------------------------
-- check the verification conditions
-- -----------------------------------------------------------------------------

constructQuery :: G r => Horns -> Sem r FInfo
constructQuery horns = generateFInfo & evalState initialState & runReader horns

generateFInfo :: FD r => Sem r FInfo
generateFInfo = do
  ask >>= traverse_ generateConstraint
  ask >>= traverse_ generateWFConstraint
  asks afQualifiers >>= traverse_ generateQualifiers
  toFInfo

generateConstraint :: FD r => Horn () -> Sem r ()
generateConstraint Horn {..} = do
  (env, (bodyExpr, headExpr)) <-
    (,)
    <$> convertExpr hornBody
    <*> convertExpr hornHead
    &   runState FT.emptyIBindEnv

  n <- freshConstraintId
  let body = mkInt bodyExpr
      hd   = mkInt headExpr
      md   = HornClauseId hornStmtId hornType
      hc   = FT.mkSubC env body hd (Just n) [] md
  modify ((hornConstraints . at n) ?~ hc)

generateWFConstraint :: FD r => Horn () -> Sem r ()
generateWFConstraint Horn {..}
  | hornType == Next = do
    (ienv, _) <- convertExpr hornBody & runLazyState mempty
    case FT.wfC ienv (mkInt e) md of
      [wf] -> modify ((wellFormednessConstraints . at kvar) ?~ wf)
      wfcs -> throw $ "did not get only 1 wfc: " ++ show wfcs
  | otherwise = return ()
 where
  kvar = FT.intKvar $ toInteger hornStmtId
  e    = FT.PKVar kvar mempty
  md   = HornClauseId hornStmtId WellFormed

generateQualifiers :: Qualifier () -> Sem r ()
generateQualifiers QImplies {..} = undefined
generateQualifiers QIff {..}     = undefined
generateQualifiers QPairs {..}   = undefined
generateQualifiers QAssume {..}  = undefined

convertExpr :: FDC r => HornExpr -> Sem r FT.Expr
convertExpr (HConstant c) = do
  globals <- gets (^. globalConstantLiterals)
  unless (FT.memberSEnv sym globals)
    $ modify (globalConstantLiterals %~ FT.insertSEnv sym FT.intSort)
  return $ FT.ESym $ FT.SL constName
 where
  constName = "const_" <> c
  sym       = symbol constName
convertExpr (HBool b)     = return $ FT.prop b
convertExpr (HInt  i)     = return $ FT.expr i
convertExpr var@HVar {..} = do
  n <- getVariableId var
  modify (FT.insertsIBindEnv [n])
  return $ FT.eVar (getFixpointName var)
convertExpr (HAnd es) = case es of
  SQ.Empty -> return FT.PTrue
  _        -> FT.PAnd . toList <$> traverse convertExpr es
convertExpr (HOr es) = case es of
  SQ.Empty -> return FT.PFalse
  _        -> FT.POr . toList <$> traverse convertExpr es
convertExpr HBinary {..} = case hBinaryOp of
  HEquals  -> FT.EEq <$> convertExpr hBinaryLhs <*> convertExpr hBinaryRhs
  HImplies -> FT.PImp <$> convertExpr hBinaryLhs <*> convertExpr hBinaryRhs
convertExpr HNot {..} = FT.PNot <$> convertExpr hNotArg
convertExpr HApp {..} = do
  functionName <- freshUF
  let fsym = symbol functionName
  modify (globalConstantLiterals %~ FT.insertSEnv fsym sort)
  FT.mkEApp (FT.dummyLoc fsym) . toList <$> traverse convertExpr hAppArgs
 where
  arity = SQ.length hAppArgs
  sort  = if arity > 0
    then FT.mkFFunc 0 (replicate (arity + 1) FT.intSort)
    else FT.intSort
convertExpr KVar {..} =
  FT.PKVar (FT.intKvar $ toInteger hKVarId)
    .   FT.mkSubst
    .   toList
    <$> traverse
          (\(lhs, rhs) -> do
            lhs' <- convertExpr lhs
            sym  <- case lhs' of
              FT.EVar v -> return v
              _ ->
                throw
                  $  "expecting lhs of kvar substitution to be a symbol: "
                  ++ show lhs
            rhs' <- convertExpr rhs
            return (sym, rhs')
          )
          hKVarSubs


getVariableId :: FD r => HornExpr -> Sem r FT.BindId
getVariableId v = do
  mid <- HM.lookup name <$> gets (^. invBindMap)
  case mid of
    Just n  -> return n
    Nothing -> do
      be <- gets (^. bindEnvironment)
      let (n, be') = FT.insertBindEnv (symbol name) sr be
      modify (bindEnvironment .~ be')
      return n
 where
  name = getFixpointName v
  sr   = case hVarType v of
    Value -> mkInt FT.PTrue
    Tag   -> mkBool FT.PTrue

getFixpointName :: HornExpr -> Id
getFixpointName HVar {..} = prefix <> name <> suffix
 where
  prefix = case (hVarType, hVarRun) of
    (Tag  , LeftRun ) -> "LT_"
    (Tag  , RightRun) -> "RT_"
    (Value, LeftRun ) -> "LV_"
    (Value, RightRun) -> "RV_"
  name   = "M_" <> hVarModule <> "_V_" <> hVarName
  suffix = if hVarIndex > 0 then "_" <> T.pack (show hVarIndex) else ""
getFixpointName _ = error "getFixpointName must be called with a variable"

-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

toFInfo :: FD r => Sem r FInfo
toFInfo =
  FT.fi
    <$> (toList <$> gets (^. hornConstraints))
    <*> (toList <$> gets (^. wellFormednessConstraints))
    <*> gets (^. bindEnvironment)
    <*> gets (^. globalConstantLiterals)
    <*> -- distinct constant symbols
        return mempty
    <*> -- set of kvars not to eliminate
        return mempty
    <*> (toList <$> gets (^. qualifiers))
    <*> -- metadata about binders
        return mempty
    <*> -- allow higher order binds?
        return False
    <*> -- allow higher order quals?
        return False
    <*> -- asserts
        return mempty
    <*> -- axiom environment
        return mempty
    <*> -- user defined data declarations
        return mempty

type FInfo = FT.FInfo HornClauseId
type Horns = L (Horn ())

type G r = Members '[Trace, Reader (AnnotationFile ()), PE.Error QueryError] r
type FD r = (G r, Members '[State St, Reader Horns] r)
type FDC r = (FD r, Member (State FT.IBindEnv) r)

initialState :: St
initialState = St mempty mempty mempty mempty mempty 0 0 mempty

freshConstraintId :: FD r => Sem r Integer
freshConstraintId =
  gets (^. constraintCounter) <* modify (& constraintCounter +~ 1)

freshUF :: FD r => Sem r Id
freshUF = do
  n <- gets (^. uninterpretedFuncCounter)
  modify (& uninterpretedFuncCounter +~ 1)
  return $ "uf_" <> T.pack (show n)

instance FT.Loc HornClauseId where
  srcSpan _ = FT.dummySpan

instance FT.Fixpoint HornClauseId where
  toFix (HornClauseId n t) =
    PP.parens (FT.toFix t) PP.<+> PP.text "stmt id:" PP.<+> PP.int n

instance NFData HornClauseId

symbol :: Id -> FT.Symbol
symbol = FT.symbol

mkInt :: FT.Expr -> FT.SortedReft
mkInt e = FT.RR FT.intSort (FT.reft (FT.vv Nothing) e)

mkBool :: FT.Expr -> FT.SortedReft
mkBool e = FT.RR FT.boolSort (FT.reft (FT.vv Nothing) e)

newtype QueryError = QueryError String
                     deriving (Show)

throw :: Member (PE.Error QueryError) r => String -> Sem r a
throw = PE.throw . QueryError
