{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import           Text.Printf

import           Iodine.Language.Annotation
import           Iodine.Language.Types
import           Iodine.Language.IR
import           Iodine.Transform.Horn
import           Iodine.Transform.VCGen         ( getVariables )

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
             , _qualifierCounter          :: Int
             , _ufCounter                 :: Int
             , _invBindMap                :: HM.HashMap Id FT.BindId
             }

makeLenses ''St


-- -----------------------------------------------------------------------------
-- generate a query for the liquid-fixpoint solver
-- -----------------------------------------------------------------------------

constructQuery :: G r => L (Module Int) -> Horns -> Sem r FInfo
constructQuery modules horns =
  generateFInfo & evalState initialState & runReader horns & runReader modules


generateFInfo :: FD r => Sem r FInfo
generateFInfo = do
  setConstants
  ask >>= traverse_ generateConstraint
  ask >>= traverse_ generateWFConstraints
  asks afQualifiers >>= traverse_ generateQualifiers
  ask >>= generateAutoQualifiers
  toFInfo

setConstants :: FD r => Sem r ()
setConstants = forM_ constants $ uncurry addBinding
 where
  b val = mkBool (FT.PIff (FT.eVar $ vSymbol) val)
  constants = [("tru", b FT.PTrue), ("fals", b FT.PFalse)]

-- -----------------------------------------------------------------------------
-- generate constraints
-- -----------------------------------------------------------------------------

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


generateWFConstraints :: FD r => Module Int -> Sem r ()
generateWFConstraints Module {..} =
  traverse_ (generateWFConstraint moduleName) gateStmts
    >> traverse_ (generateWFConstraint moduleName . abStmt) alwaysBlocks

generateWFConstraint :: FD r => Id -> Stmt Int -> Sem r ()
generateWFConstraint hVarModule stmt = do
  (ienv, _) <- traverse_ convertExpr hvars & runState mempty
  case FT.wfC ienv (mkInt e) md of
    [wf] -> modify ((wellFormednessConstraints . at kvar) ?~ wf)
    wfcs -> throw $ "did not get only 1 wfc: " ++ show wfcs
 where
  vars  = getVariables stmt
  hvars = foldl'
    (\acc hVarName ->
      acc
        |> HVar { hVarIndex = 0, hVarType = Value, hVarRun = LeftRun, .. }
        |> HVar { hVarIndex = 0, hVarType = Value, hVarRun = RightRun, .. }
        |> HVar { hVarIndex = 0, hVarType = Tag, hVarRun = LeftRun, .. }
        |> HVar { hVarIndex = 0, hVarType = Tag, hVarRun = RightRun, .. }
    )
    SQ.empty
    vars
  stmtId = stmtData stmt
  kvar   = mkKVar stmtId
  e      = FT.PKVar kvar mempty
  md     = HornClauseId stmtId WellFormed

convertExpr :: FDC r => HornExpr -> Sem r FT.Expr
convertExpr (HConstant c) = do
  globals <- gets (^. globalConstantLiterals)
  unless (FT.memberSEnv sym globals)
    $ modify (globalConstantLiterals %~ FT.insertSEnv sym FT.intSort)
  return $ FT.ESym $ FT.SL constName
 where
  constName = "const_" <> c
  sym       = symbol constName

convertExpr (HBool b) = do
  be <- gets (^. invBindMap)
  let n = be HM.! name
  modify (FT.insertsIBindEnv [n])
  return $ FT.eVar name
  where name = if b then "tru" else "fals"

convertExpr (HInt i) = getIntSymbol i

convertExpr var@HVar {..} = do
  n <- getVariableId var
  modify (FT.insertsIBindEnv [n])
  return $ FT.eVar (getFixpointName var)

convertExpr (HAnd es) = case es of
  SQ.Empty -> convertExpr (HBool True)
  _        -> FT.PAnd . toList <$> traverse convertExpr es

convertExpr (HOr es) = case es of
  SQ.Empty -> convertExpr (HBool False)
  _        -> FT.POr . toList <$> traverse convertExpr es

convertExpr HBinary {..} = case hBinaryOp of
  HEquals  -> FT.EEq <$> convertExpr hBinaryLhs <*> convertExpr hBinaryRhs
  HImplies -> FT.PImp <$> convertExpr hBinaryLhs <*> convertExpr hBinaryRhs
  HIff     -> FT.PIff <$> convertExpr hBinaryLhs <*> convertExpr hBinaryRhs

convertExpr HNot {..} = FT.PNot <$> convertExpr hNotArg

convertExpr HApp {..} = do
  fsym <- case hAppMFun of
    Just f  -> return $ symbol f
    Nothing -> do
      n <- gets (^. ufCounter) <* modify (& ufCounter +~ 1)
      return . symbol $ "uf_noname_" <> T.pack (show n)
  modify (globalConstantLiterals %~ FT.insertSEnv fsym sort)
  FT.mkEApp (FT.dummyLoc fsym) . toList <$> traverse convertExpr hAppArgs
 where
  arity = SQ.length hAppArgs
  sort  = if arity > 0
    then FT.mkFFunc 0 (replicate (arity + 1) FT.intSort)
    else FT.intSort

convertExpr KVar {..} =
  FT.PKVar (mkKVar hKVarId)
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
    Nothing -> addBinding name sr
 where
  name = getFixpointName v
  sr   = case hVarType v of
    Value -> mkInt FT.PTrue
    Tag   -> mkBool FT.PTrue

addBinding :: FD r => Id -> FT.SortedReft -> Sem r FT.BindId
addBinding name sr = do
  be <- gets (^. bindEnvironment)
  let (n, be') = FT.insertBindEnv (symbol name) sr be
  modify (bindEnvironment .~ be')
  modify (invBindMap . at name ?~ n)
  return n

getIntSymbol :: FDC r => Int -> Sem r FT.Expr
getIntSymbol i = do
  be <- gets (^. invBindMap)
  n  <- case HM.lookup name be of
    Just n -> return n
    Nothing ->
      let sr = mkInt (FT.EEq (FT.eVar vSymbol) (FT.ECon $ FT.I $ toInteger i))
      in  addBinding name sr
  modify (FT.insertsIBindEnv [n])
  return $ FT.eVar name
  where name = "number_" <> T.pack (show i)

-- -----------------------------------------------------------------------------
-- generate qualifiers
-- -----------------------------------------------------------------------------

generateQualifiers :: FD r => Qualifier () -> Sem r ()
generateQualifiers QImplies {..} = do
  m <- asks afTopModule
  q m LeftRun <$> freshQualifierId >>= addQualifier
  q m RightRun <$> freshQualifierId >>= addQualifier
 where
  mkVar hVarName hVarModule hVarRun =
    getFixpointName HVar { hVarIndex = 0, hVarType = Tag, .. }
  mkL = mkVar qualifierLhs
  mkRs hVarModule hVarRun =
    [ mkVar r hVarModule hVarRun | r <- toList qualifierRhss ]
  q hVarModule runType n =
    let l  = mkL hVarModule runType
        rs = mkRs hVarModule runType
    in  FT.mkQual
          (FT.symbol $ "Custom1_" ++ show n)
          (  [ FT.QP vSymbol FT.PatNone FT.FInt
             , FT.QP (symbol l) (FT.PatExact (symbol l)) FT.boolSort
             ]
          ++ [ FT.QP (symbol r) (FT.PatExact (symbol r)) FT.boolSort
             | r <- rs
             ]
          )
          (FT.PImp (FT.eVar @Id l) $ FT.POr [ FT.eVar @Id v | v <- rs ])
          (FT.dummyPos "")

generateQualifiers QPairs {..} = varPairs <$> asks afTopModule >>= traverse_
  (\pair -> q pair <$> freshQualifierId >>= addQualifier)
 where
  vars hVarModule =
    (\hVarName -> HVar { hVarIndex = 0, hVarRun = LeftRun, hVarType = Tag, .. })
      <$> qualifierEqs
  varPairs m = twoPairs $ getFixpointName <$> vars m
  q (x1, x2) n = FT.mkQual
    (FT.symbol $ "Custom2_" ++ show n)
    [ FT.QP vSymbol FT.PatNone FT.FInt
    , FT.QP (symbol x1) (FT.PatExact (symbol x1)) FT.boolSort
    , FT.QP (symbol x2) (FT.PatExact (symbol x2)) FT.boolSort
    ]
    (FT.PIff (FT.eVar x1) (FT.eVar x2))
    (FT.dummyPos "")

generateQualifiers QIff {..} = do
  m <- asks afTopModule
  q m LeftRun <$> freshQualifierId >>= addQualifier
  q m RightRun <$> freshQualifierId >>= addQualifier
 where
  mkVar hVarName hVarModule hVarRun =
    getFixpointName HVar { hVarIndex = 0, hVarType = Tag, .. }
  mkL = mkVar qualifierLhs
  mkRs hVarModule hVarRun =
    [ mkVar r hVarModule hVarRun | r <- toList qualifierRhss ]
  q hVarModule runType n =
    let l  = mkL hVarModule runType
        rs = mkRs hVarModule runType
    in  FT.mkQual
          (FT.symbol $ "Custom1_" ++ show n)
          (  [ FT.QP vSymbol FT.PatNone FT.FInt
             , FT.QP (symbol l) (FT.PatExact (symbol l)) FT.boolSort
             ]
          ++ [ FT.QP (symbol r) (FT.PatExact (symbol r)) FT.boolSort
             | r <- rs
             ]
          )
          (FT.PIff (FT.eVar @Id l) $ FT.POr [ FT.eVar @Id v | v <- rs ])
          (FT.dummyPos "")


defaultQualifiers :: L FT.Qualifier
defaultQualifiers =
  mempty
    |> mkEq "ValueEq" Value FT.intSort
    |> mkEq "TagEq"   Tag   FT.boolSort
    |> mkTagZero 0 LeftRun
    |> mkTagZero 1 RightRun
 where
  mkEq name t s = FT.mkQual
    (FT.symbol @String name)
    [ FT.QP vSymbol FT.PatNone FT.FInt
    , FT.QP (symbol "x") (FT.PatPrefix (symbol $ getVarPrefix t LeftRun) 1) s
    , FT.QP (symbol "y") (FT.PatPrefix (symbol $ getVarPrefix t RightRun) 1) s
    ]
    ( case t of
        Value -> FT.PAtom FT.Eq (FT.eVar @Id "x") (FT.eVar @Id "y")
        Tag   -> FT.PIff (FT.eVar @Id "x") (FT.eVar @Id "y")
    )
    (FT.dummyPos "")

  mkTagZero n r = FT.mkQual
    (FT.symbol @String (printf "TagZero%d" (n :: Int)))
    [ FT.QP vSymbol FT.PatNone FT.FInt
    , FT.QP (symbol "x")
            (FT.PatPrefix (symbol $ getVarPrefix Tag r) 1)
            (FT.FTC FT.boolFTyCon)
    ]
    (FT.PIff (FT.eVar @Id "x") FT.PFalse)
    (FT.dummyPos "")

generateAutoQualifiers :: FD r => AnnotationFile () -> Sem r ()
generateAutoQualifiers AnnotationFile {..} = forM_ sourcePairs $ \(s1, s2) ->
  mkQ (mkHVar s1 LeftRun) (mkHVar s2 LeftRun) <$> freshQualifierId >>= addQualifier
  where
    sources =
      foldl' (\acc -> \case
                 Source v _ -> acc |> v
                 _ -> acc) SQ.empty afAnnotations
    sourcePairs = twoPairs sources
    mkQ s1 s2 n =
      FT.mkQual
      (FT.symbol $ "SrcTagEq_" ++ show n)
      [ FT.QP vSymbol FT.PatNone FT.FInt
      , FT.QP (symbol s1) (FT.PatExact (symbol s1)) FT.boolSort
      , FT.QP (symbol s2) (FT.PatExact (symbol s2)) FT.boolSort
      ]
      (FT.PIff (FT.eVar s1) (FT.eVar s2))
      (FT.dummyPos "")
    mkHVar v r =
      getFixpointName $
      HVar { hVarName   = v
           , hVarModule = afTopModule
           , hVarIndex  = 0
           , hVarType   = Tag
           , hVarRun    = r
           }


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

getFixpointName :: HornExpr -> Id
getFixpointName HVar {..} = varno <> prefix <> name
 where
  varno  = if hVarIndex > 0
           then "N" <> T.pack (show hVarIndex) <> "_"
           else ""
  prefix = getVarPrefix hVarType hVarRun
  name   = "M_" <> hVarModule <> "_V_" <> hVarName
getFixpointName _ = error "getFixpointName must be called with a variable"

getVarPrefix :: HornVarType -> HornVarRun -> Id
getVarPrefix hVarType hVarRun = case (hVarType, hVarRun) of
  (Tag  , LeftRun ) -> "VLT_"
  (Tag  , RightRun) -> "VRT_"
  (Value, LeftRun ) -> "VL_"
  (Value, RightRun) -> "VR_"

type FInfo = FT.FInfo HornClauseId
type Horns = L (Horn ())

type G r = Members '[Trace, Reader (AnnotationFile ()), PE.Error QueryError] r
type FD r = (G r, Members '[State St, Reader Horns, Reader (L (Module Int))] r)
type FDC r = (FD r, Member (State FT.IBindEnv) r)

addQualifier :: FD r => FT.Qualifier -> Sem r ()
addQualifier q = modify (& qualifiers %~ (|> q))

initialState :: St
initialState = St mempty mempty mempty mempty defaultQualifiers 0 0 0 mempty

freshConstraintId :: FD r => Sem r Integer
freshConstraintId =
  gets (^. constraintCounter) <* modify (& constraintCounter +~ 1)

freshQualifierId :: FD r => Sem r Int
freshQualifierId =
  gets (^. qualifierCounter) <* modify (& qualifierCounter +~ 1)

instance FT.Loc HornClauseId where
  srcSpan _ = FT.dummySpan

instance FT.Fixpoint HornClauseId where
  toFix (HornClauseId n t) =
    PP.parens (FT.toFix t) PP.<+> PP.text "stmt id:" PP.<+> PP.int n

instance NFData HornClauseId

symbol :: Id -> FT.Symbol
symbol = FT.symbol

mkInt :: FT.Expr -> FT.SortedReft
mkInt e = FT.RR FT.intSort (FT.reft vSymbol e)

mkBool :: FT.Expr -> FT.SortedReft
mkBool e = FT.RR FT.boolSort (FT.reft vSymbol e)

newtype QueryError = QueryError String
                     deriving (Show)

throw :: Member (PE.Error QueryError) r => String -> Sem r a
throw = PE.throw . QueryError

-- return combinations of the elements
twoPairs :: L a -> L (a, a)
twoPairs SQ.Empty      = mempty
twoPairs (a SQ.:<| as) = ((a, ) <$> as) <> twoPairs as

mkKVar :: Int -> FT.KVar
mkKVar n = FT.KV . FT.symbol $ "inv" <> show n

vSymbol :: FT.Symbol
vSymbol = symbol "v"