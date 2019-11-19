{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.VCGen
  ( vcgen
  , getVariables
  , VCGenOutput
  , VCGenError(..)
  )
where

import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Language.Types
import           Iodine.Transform.Horn
import           Iodine.Transform.SSA           ( SSAIR
                                                , SSAOutput
                                                )

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as SQ
import qualified Data.Text                     as T
import           Polysemy
import qualified Polysemy.Error                as PE
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace
import           Text.Printf
import           Text.Read                      ( readEither )

-- -----------------------------------------------------------------------------
-- vcgen state
-- -----------------------------------------------------------------------------

type Ids = HS.HashSet Id

data ModuleSt = ModuleSt { _currentModule    :: Module Int -- current module
                         , _isTopModule      :: Bool -- is the current module the top module?
                         }
                deriving (Show)

data StmtSt = StmtSt { _currentVariables :: Ids -- all vars in this block
                     , _currentSources   :: Ids -- sources in this block
                     , _currentSinks     :: Ids -- sinks in this block
                     , _currentInitEqs   :: Ids -- init_eq vars in this block
                     , _currentAlwaysEqs :: Ids -- always_eq vars in this block
                     , _currentAssertEqs :: Ids -- assert_eq vars in this block
                     }
              deriving (Show)

makeLenses ''ModuleSt
makeLenses ''StmtSt

-- -----------------------------------------------------------------------------
-- vcgen implementation
-- -----------------------------------------------------------------------------

vcgen :: G r => SSAOutput -> Sem r VCGenOutput
vcgen (ssaIR, trNextVariables) =
  vcgenHelper ssaIR & runReader (NextVars trNextVariables)

vcgenHelper :: FD r => SSAIR -> Sem r VCGenOutput
vcgenHelper ssaIR = do
  unless (SQ.length ssaIR == 1) $ throw $ printf "expecting a single module"
  combine vcgenMod ssaIR

vcgenMod :: FD r => Module Int -> Sem r Horns
vcgenMod Module {..} = do
  isTop <- (moduleName ==) <$> asks afTopModule
  let moduleSt = ModuleSt Module { .. } isTop
  combine regularChecks allStmts
    <||> interferenceChecks allStmts
    &    runReader moduleSt
  where allStmts = gateStmts SQ.>< (abStmt <$> alwaysBlocks)

regularChecks :: FDM r => S -> Sem r Horns
regularChecks s =
  withStmt s
    $    pure SQ.empty
    ||>  initialize s
    ||>  tagReset s
    ||>  srcReset s
    ||>  next s
    <||> sinkCheck s
    <||> assertEqCheck s


-- -----------------------------------------------------------------------------
-- 1. initialize
-- -----------------------------------------------------------------------------
-- Initially, all variables are untagged. We assume that init_eq or
-- always_eq variables are equal to each other in two runs.
-- -----------------------------------------------------------------------------

initialize :: FDS r => S -> Sem r (Horn ())
initialize stmt = do
  Module {..} <- asks (^. currentModule)
  subs1 <- foldl' (zeroTags moduleName) mempty <$> asks (^. currentVariables) -- untag everything
  subs2 <- foldl' (valEquals moduleName) subs1 <$> asks (^. currentInitEqs)   -- init_eq vars are equal
  subs <- foldl' (valEquals moduleName) subs2 <$> asks (^. currentAlwaysEqs) -- always_eq vars are equal
  return $ Horn { hornHead   = KVar stmtId subs
                , hornBody   = HBool True
                , hornType   = Init
                , hornStmtId = stmtId
                , hornData   = ()
                }
 where
  stmtId = stmtData stmt
  zeroTags m subs v =
    subs
      |> (HVar v m 0 Tag LeftRun , HBool False)
      |> (HVar v m 0 Tag RightRun, HBool False)
  valEquals m subs v =
    subs |> (HVar v m 0 Value LeftRun, HVar v m 0 Value RightRun)


-- -----------------------------------------------------------------------------
-- 2. tag reset
-- -----------------------------------------------------------------------------
-- We set the tags of the source variables, and unset the tags of everything else.
-- -----------------------------------------------------------------------------

tagReset :: FDS r => S -> Sem r (Horn ())
tagReset stmt = do
  Module {..} <- asks (^. currentModule)
  srcs        <- asks (^. currentSources)
  vars        <- asks (^. currentVariables)
  let non_srcs = HS.difference vars srcs
  let subs1    = foldl' (tags moduleName True) mempty srcs    -- sources are tagged
  let subs     = foldl' (tags moduleName False) subs1 non_srcs -- non sources are untagged
  return $ Horn { hornHead   = KVar stmtId subs
                , hornBody   = KVar stmtId mempty
                , hornType   = TagReset
                , hornStmtId = stmtId
                , hornData   = ()
                }

 where
  stmtId = stmtData stmt
  tags m value subs v =
    subs
      |> (HVar v m 0 Tag LeftRun , HBool value)
      |> (HVar v m 0 Tag RightRun, HBool value)


-- -----------------------------------------------------------------------------
-- 3. src reset
-- -----------------------------------------------------------------------------
-- We unset the tags of the source variables.
-- -----------------------------------------------------------------------------

srcReset :: FDS r => S -> Sem r (Horn ())
srcReset stmt = do
  Module {..} <- asks (^. currentModule)
  subs <- foldl' (tags moduleName False) mempty <$> asks (^. currentSources) -- sources are untagged
  return $ Horn { hornHead   = KVar stmtId subs
                , hornBody   = KVar stmtId mempty
                , hornType   = SourceReset
                , hornStmtId = stmtId
                , hornData   = ()
                }

 where
  stmtId = stmtData stmt
  tags m value subs v =
    subs
      |> (HVar v m 0 Tag LeftRun , HBool value)
      |> (HVar v m 0 Tag RightRun, HBool value)


-- -----------------------------------------------------------------------------
-- 4. transition relation
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

next :: FDS r => S -> Sem r (Horn ())
next stmt = do
  Module {..} <- asks (^. currentModule)
  nextVars    <- (IM.! stmtId) <$> asks getNextVars
  equalities  <- foldl' (ae moduleName nextVars) mempty
    <$> asks (^. currentAlwaysEqs)
  trace $ show ("equalities", equalities)
  let subs = toSubs moduleName nextVars
  return $ Horn { hornBody   = HAnd $ (KVar stmtId mempty |:> tr) <> equalities
                , hornHead   = KVar stmtId subs
                , hornType   = Next
                , hornStmtId = stmtId
                , hornData   = ()
                }
 where
  stmtId = stmtData stmt
  tr     = transitionRelation stmt
  ae m nvs exprs v =
    let exprs' = exprs |> HBinary HEquals
                                  (HVar v m 0 Value LeftRun)
                                  (HVar v m 0 Value RightRun)
    in  case HM.lookup v nvs of
          Just n -> exprs' |> HBinary HEquals
                                      (HVar v m n Value LeftRun)
                                      (HVar v m n Value RightRun)
          Nothing -> exprs'

transitionRelation :: S -> HornExpr
transitionRelation s =
  HAnd $ transitionRelation' LeftRun s |:> transitionRelation' RightRun s

transitionRelation' :: HornVarRun -> S -> HornExpr
transitionRelation' r = \case
  Block {..} -> HAnd $ transitionRelation' r <$> blockStmts
  Assignment {..} ->
    HAnd $ HBinary HEquals (valE assignmentLhs) (valE assignmentRhs) |:> HBinary
      HEquals
      (tagE assignmentLhs)
      (tagE assignmentRhs)
  IfStmt {..} ->
    let not_c = HBinary HEquals (valE ifStmtCondition) (HInt 0)
        t = transitionRelation' r ifStmtThen
        e = transitionRelation' r ifStmtElse
    in  HOr $ HBinary HImplies (HNot not_c) t |:> HBinary HImplies not_c e
  ModuleInstance {..} -> error "submodules are not supported"
  PhiNode {..} ->
    let lhsValue = valE phiLhs
        lhsTag   = tagE phiLhs
    in  HAnd $ HOr (HBinary HEquals lhsValue . valE <$> phiRhs) |:> HOr
          (HBinary HEquals lhsTag . tagE <$> phiRhs)
  Skip {..} -> HAnd mempty
 where
  ufVal :: Maybe Id -> L (Expr Int) -> HornExpr
  ufVal fname = HApp fname . fmap valE

  ufTag :: L (Expr Int) -> HornExpr
  ufTag = HOr . fmap tagE

  valE :: Expr Int -> HornExpr
  valE = \case
    Constant {..} -> parseVerilogInt constantValue
    Variable {..} -> HVar { hVarName   = varName
                          , hVarModule = varModuleName
                          , hVarIndex  = exprData
                          , hVarType   = Value
                          , hVarRun    = r
                          }
    UF {..}     -> ufVal (Just ufName) ufArgs
    IfExpr {..} -> ufVal Nothing (ifExprCondition |:> ifExprThen |> ifExprElse)
    Str {..}    -> error "Strings are not handled (yet)"
    Select {..} -> ufVal Nothing (selectVar <| selectIndices)

  tagE :: Expr Int -> HornExpr
  tagE = \case
    Constant {..} -> HBool False
    Variable {..} -> HVar { hVarName   = varName
                          , hVarModule = varModuleName
                          , hVarIndex  = exprData
                          , hVarType   = Tag
                          , hVarRun    = r
                          }
    UF {..}     -> ufTag ufArgs
    IfExpr {..} -> ufTag (ifExprCondition |:> ifExprThen |> ifExprElse)
    Str {..}    -> HBool False
    Select {..} -> ufTag (selectVar <| selectIndices)

parseVerilogInt :: Id -> HornExpr
parseVerilogInt value = case readEither v' of
  Left  _ -> HConstant value
  Right n -> HInt n
 where
  v  = T.unpack value
  v' = case v of
    '0' : 'b' : rst -> rst
    _               -> v


-- -----------------------------------------------------------------------------
-- 5. sink check
-- -----------------------------------------------------------------------------
-- Checks that variables defined as sinks are tainted equally.
-- -----------------------------------------------------------------------------

sinkCheck :: FDS r => S -> Sem r Horns
sinkCheck stmt = do
  Module {..} <- asks (^. currentModule)
  sinks       <- asks (^. currentSinks)
  return $ foldl' (\hs v -> hs |> go moduleName v) mempty sinks
 where
  stmtId = stmtData stmt
  go m v = Horn
    { hornHead   = HBinary HEquals
                           (HVar v m 0 Tag LeftRun)
                           (HVar v m 0 Tag RightRun)
    , hornBody   = KVar stmtId mempty
    , hornType   = TagEqual
    , hornStmtId = stmtId
    , hornData   = ()
    }


-- -----------------------------------------------------------------------------
-- 6. assert eq check
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

assertEqCheck :: FDS r => S -> Sem r Horns
assertEqCheck stmt = do
  Module {..} <- asks (^. currentModule)
  aes         <- asks (^. currentAssertEqs)
  return $ foldl' (\hs v -> hs |> go moduleName v) mempty aes
 where
  stmtId = stmtData stmt
  go m v = Horn
    { hornHead   = HBinary HEquals
                           (HVar v m 0 Value LeftRun)
                           (HVar v m 0 Value RightRun)
    , hornBody   = KVar stmtId mempty
    , hornType   = AssertEqCheck
    , hornStmtId = stmtId
    , hornData   = ()
    }


-- -----------------------------------------------------------------------------
-- 7. interference checks
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

data ICSt = ICSt { icStmt :: S,  writtenVars :: Ids, allVars :: Ids }
type ICSts = IM.IntMap ICSt

interferenceChecks :: FDM r => Ss -> Sem r Horns
interferenceChecks stmts =
  (traverse_ interferenceCheck stmts >> get @Horns)
    & evalState @ICSts mempty
    & evalState @Horns mempty

interferenceCheck
  :: (FDM r, Members '[State ICSts, State Horns] r) => S -> Sem r ()
interferenceCheck stmt = do
  -- get the statements we have looked at so far
  icSts <- get @ICSts
  traverse_
    (\icSt@ICSt {..} -> do
      when (currentWrittenVars `intersects` allVars) $ do
        h <- interferenceCheckWR currentSt icSt
        modify @Horns (|> h)
      when (writtenVars `intersects` currentAllVars) $ do
        h <- interferenceCheckWR icSt currentSt
        modify @Horns (|> h)
    )
    icSts
  modify $ IM.insert stmtId currentSt
 where
  stmtId    = stmtData stmt
  currentSt = ICSt { icStmt      = stmt
                   , writtenVars = currentWrittenVars
                   , allVars     = currentAllVars
                   }
  currentAllVars     = getVariables stmt
  currentWrittenVars = getUpdatedVariables stmt

-- return the write/read interference check
interferenceCheckWR :: FDM r => ICSt -> ICSt -> Sem r (Horn ())
interferenceCheckWR wSt rSt = do
  Module {..} <- asks (^. currentModule)
  wNext       <- (IM.! stmtData wStmt) <$> asks getNextVars
  let subs = toSubs moduleName
        $ HM.filterWithKey (\var _ -> HS.member var rVars) wNext
      rId = stmtData rStmt
      wId = stmtData wStmt
  return $ Horn
    { hornHead   = KVar rId subs
    , hornBody   = HAnd
                   $   KVar rId mempty
                   |:> KVar wId mempty
                   |>  transitionRelation wStmt
    , hornType   = Interference
    , hornStmtId = wId
    , hornData   = ()
    }
 where
  rStmt = icStmt rSt
  wStmt = icStmt wSt
  rVars = allVars rSt



-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

type S = Stmt Int
type Ss = L S
type Horns = L (Horn ())

type VCGenOutput = Horns

newtype NextVars = NextVars { getNextVars :: IM.IntMap (HM.HashMap Id Int) }
type AF = AnnotationFile ()

type G r = Members '[Reader AF, PE.Error VCGenError, Trace] r

type FD r = (G r, Members '[Reader NextVars] r)
type FDM r = (G r, Members '[Reader ModuleSt, Reader NextVars] r)
type FDS r = (G r, Members '[Reader ModuleSt, Reader StmtSt, Reader NextVars] r)

infixl 9 ||>
(||>) :: Applicative f => f (L a) -> f a -> f (L a)
(||>) fas fa = (|>) <$> fas <*> fa

infixl 9 <||>
(<||>) :: Applicative f => f (L a) -> f (L a) -> f (L a)
(<||>) = liftA2 (<>)

(|:>) :: (Snoc s s a a, Monoid s) => a -> a -> s
(|:>) a1 a2 = mempty |> a1 |> a2

withStmt :: FDM r => S -> Sem (Reader StmtSt ': r) a -> Sem r a
withStmt s act = do
  stmtSt <- computeStmtSt s
  act & runReader stmtSt

computeStmtSt :: FDM r => S -> Sem r StmtSt
computeStmtSt stmt = do
  as          <- asks afAnnotations
  Module {..} <- asks (^. currentModule)
  isTop       <- asks (^. isTopModule)
  (do
      modify $ currentVariables .~ vs
      let addIf v setter = when (HS.member v vs) $ modify setter
      trace $ show ("isTop", isTop)
      trace $ show ("as", as)
      trace $ show ("vs", vs)
      for_ as $ \case
        Source   s  _ -> when isTop $ addIf s (currentSources %~ HS.insert s)
        Sink     s  _ -> when isTop $ addIf s (currentSinks %~ HS.insert s)
        Sanitize ss _ -> when isTop
          $ traverse_ (\s -> addIf s (currentInitEqs %~ HS.insert s)) ss
        SanitizeMod m v _ ->
          when (moduleName == m) $ addIf v (currentInitEqs %~ HS.insert v)
        SanitizeGlob s _ ->
          when isTop $ addIf s (currentAlwaysEqs %~ HS.insert s)
        AssertEq v _ -> when isTop $ addIf v (currentAssertEqs %~ HS.insert v)
    )
    & runState (StmtSt mempty mempty mempty mempty mempty mempty)
    & fmap fst
  where vs = getVariables stmt

-- unsetAnnotations :: FD r => Sem r ()
-- unsetAnnotations = do
--   modify $ currentVariables .~ mempty
--   modify $ currentSources .~ mempty
--   modify $ currentSinks .~ mempty
--   modify $ currentInitEqs .~ mempty
--   modify $ currentAlwaysEqs .~ mempty
--   modify $ currentAssertEqs .~ mempty

getVariables :: Stmt a -> Ids
getVariables = \case
  Block {..}      -> mfold getVariables blockStmts
  Assignment {..} -> mfold go [assignmentLhs, assignmentRhs]
  IfStmt {..} ->
    go ifStmtCondition <> mfold getVariables [ifStmtThen, ifStmtElse]
  ModuleInstance {..} -> mempty
  PhiNode {..}        -> mempty
  Skip {..}           -> mempty
 where
  go :: Expr a -> Ids
  go Variable {..} = HS.singleton varName
  go Constant {..} = mempty
  go UF {..}       = mfold go ufArgs
  go IfExpr {..}   = mfold go [ifExprCondition, ifExprThen, ifExprElse]
  go Str {..}      = mempty
  go Select {..}   = go selectVar <> mfold go selectIndices

getUpdatedVariables :: Stmt a -> Ids
getUpdatedVariables = \case
  Block {..}          -> mfold getUpdatedVariables blockStmts
  Assignment {..}     -> HS.singleton $ varName assignmentLhs
  IfStmt {..}         -> mfold getVariables [ifStmtThen, ifStmtElse]
  ModuleInstance {..} -> mempty
  PhiNode {..}        -> mempty
  Skip {..}           -> mempty

toSubs :: Id -> HM.HashMap Id Int -> L (HornExpr, HornExpr)
toSubs m = HM.foldlWithKey' go mempty
 where
  go subs v n =
    subs
      |> (HVar v m 0 Tag LeftRun   , HVar v m n Tag LeftRun)
      |> (HVar v m 0 Value LeftRun , HVar v m n Value LeftRun)
      |> (HVar v m 0 Tag RightRun  , HVar v m n Tag RightRun)
      |> (HVar v m 0 Value RightRun, HVar v m n Value RightRun)

-- -----------------------------------------------------------------------------
-- other stuff
-- -----------------------------------------------------------------------------

newtype VCGenError = VCGenError String
                     deriving (Show)

throw :: G r => String -> Sem r a
throw = PE.throw . VCGenError

combine :: (Monoid m, Traversable t) => (a -> Sem r m) -> t a -> Sem r m
combine act as = foldl' (<>) mempty <$> traverse act as

mfold :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mfold f = foldl' (\ms a -> f a <> ms) mempty

intersects :: HS.HashSet Id -> HS.HashSet Id -> Bool
intersects s1 s2 = go (HS.toList s1)
 where
  go []       = False
  go (a : as) = HS.member a s2 || go as

