{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.VCGen
  ( vcgen
  , getVariables
  , VCGenOutput
  )
where

import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Transform.Horn
import           Iodine.Transform.SSA (SSAOutput)
import           Iodine.Types
import           Iodine.Utils

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.IntMap                   as IM
import           Data.List (nub)
import qualified Data.Sequence                 as SQ
import qualified Data.Text                     as T
import           Polysemy
import qualified Polysemy.Error                as PE
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace
import           Text.Printf
import           Text.Read (readEither)

import qualified Debug.Trace as DT


-- -----------------------------------------------------------------------------
-- vcgen state
-- -----------------------------------------------------------------------------

type Ids = HS.HashSet Id

data ModuleSt = ModuleSt { _currentModule :: Module Int -- | current module
                         , _isTopModule   :: Bool       -- | is this the top (or root) module?
                         }
                deriving (Show)

data StmtSt = StmtSt { _currentVariables :: Ids -- | all vars in this block
                     , _currentSources   :: Ids -- | sources in this block
                     , _currentSinks     :: Ids -- | sinks in this block
                     , _currentInitEqs   :: Ids -- | init_eq vars in this block
                     , _currentAlwaysEqs :: Ids -- | always_eq vars in this block
                     , _currentAssertEqs :: Ids -- | assert_eq vars in this block
                     }
              deriving (Show)

makeLenses ''ModuleSt
makeLenses ''StmtSt

-- -----------------------------------------------------------------------------
-- vcgen implementation
-- -----------------------------------------------------------------------------

vcgen :: G r => SSAOutput -> Sem r VCGenOutput
vcgen (ssaIR, trNextVariables) =
  runReader (NextVars trNextVariables) $
  do unless (SQ.length ssaIR == 1) $
       throw $ printf "expecting a single module"
     combine vcgenMod ssaIR

vcgenMod :: FD r => Module Int -> Sem r Horns
vcgenMod Module {..} = do
  isTop <- (moduleName ==) <$> asks afTopModule
  let moduleSt = ModuleSt Module { .. } isTop

  assert (SQ.null gateStmts) $
    "Gate statements should have been merged into always* blocks"
  assert singleBlockForEvent $
    "There should be at most one always block for each event type"

  combine regularChecks alwaysBlocks <||> interferenceChecks allStmts
    & runReader moduleSt

  where
    allStmts  = abStmt <$> alwaysBlocks
    allEvents = fmap (const ()) <$>
                toList (abEvent <$> alwaysBlocks)
    singleBlockForEvent =
      length allEvents == length (nub allEvents)

regularChecks :: FDM r => AlwaysBlock Int -> Sem r Horns
regularChecks AlwaysBlock{..} =
  withStmt abStmt
    $    pure SQ.empty
    ||>  initialize abStmt
    ||>  tagReset abStmt abEvent
    ||>  srcReset abStmt abEvent
    ||>  next abStmt
    <||> sinkCheck abStmt
    <||> assertEqCheck abStmt


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

tagReset :: FDS r => S -> Event Int -> Sem r (Horn ())
tagReset stmt event = do
  Module {..} <- asks (^. currentModule)
  srcs        <- asks (^. currentSources)
  vars        <- asks (^. currentVariables)
  let non_srcs = HS.difference vars srcs
  let subs0 = foldl' (tags moduleName True) mempty srcs     -- sources are tagged
  let subs  = foldl' (tags moduleName False) subs0 non_srcs -- non sources are untagged
  case event of
    Star _ -> do
      aes <- alwaysEqualEqualities stmt
      nextVars  <- (IM.! stmtId) <$> asks getNextVars
      let tagSubs = toSubsTags moduleName nextVars
      let initEqs = HAnd $ (uncurry2 $ HBinary HIff) <$> subs
      return $
        Horn { hornHead   = KVar stmtId tagSubs
             , hornBody   = HAnd $
                            (KVar stmtId mempty <| aes) |>
                            initEqs |>
                            transitionRelation stmt
             , hornType   = TagReset
             , hornStmtId = stmtId
             , hornData   = ()
             }
    _ ->
      return $
      Horn { hornHead   = KVar stmtId subs
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

srcReset :: FDS r => S -> Event Int -> Sem r (Horn ())
srcReset stmt event = do
  Module {..} <- asks (^. currentModule)
  subs <- foldl' (tags moduleName False) mempty <$> asks (^. currentSources) -- sources are untagged
  case event of
    Star _ -> do
      aes <- alwaysEqualEqualities stmt
      nextVars  <- (IM.! stmtId) <$> asks getNextVars
      let tagSubs = toSubsTags moduleName nextVars
      let initEqs = HAnd $ (uncurry2 $ HBinary HIff) <$> subs
      return $
        Horn { hornHead   = KVar stmtId tagSubs
             , hornBody   = HAnd $
                            (KVar stmtId mempty <| aes) |>
                            initEqs |>
                            transitionRelation stmt
             , hornType   = SourceReset
             , hornStmtId = stmtId
             , hornData   = ()
             }
    _ ->
      return $
      Horn { hornHead   = KVar stmtId subs
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
  nextVars <- (IM.! stmtId) <$> asks getNextVars
  equalities <- alwaysEqualEqualities stmt
  trace $ show ("equalities" :: String, equalities)
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


alwaysEqualEqualities :: FDS r => S -> Sem r (L HornExpr)
alwaysEqualEqualities stmt = do
  Module {..} <- asks (^. currentModule)
  nextVars <- (IM.! stmtId) <$> asks getNextVars
  foldl' (go moduleName nextVars) mempty <$>
    asks (^. currentAlwaysEqs)
  where
    stmtId = stmtData stmt
    go m nvs exprs v =
      let exprs' =
            exprs |>
            HBinary HEquals (HVar v m 0 Value LeftRun) (HVar v m 0 Value RightRun) |>
            HBinary HIff (HVar v m 0 Tag LeftRun) (HVar v m 0 Tag RightRun)
      in case HM.lookup v nvs of
           Just n -> exprs' |>
                     HBinary HEquals (HVar v m n Value LeftRun) (HVar v m n Value RightRun)
           Nothing -> exprs'


transitionRelation :: S -> HornExpr
transitionRelation s =
  HAnd $
  transitionRelation' mempty LeftRun s |:>
  transitionRelation' mempty RightRun s

type PathCond = L (Expr Int)

transitionRelation' :: PathCond -> HornVarRun -> S -> HornExpr
transitionRelation' conds r = \case
  Block {..} -> HAnd $ transitionRelation' conds r <$> blockStmts
  Assignment {..} -> HAnd $
    HBinary HEquals (val assignmentLhs) (val assignmentRhs) |:>
    let result = HBinary HIff (tag assignmentLhs) (tagWithCond conds assignmentRhs)
    in case r of
         LeftRun ->
           dt_trace
           (printf "%s <- %s" (show $ tag assignmentLhs) (show $ tagWithCond conds assignmentRhs))
           result
         RightRun -> result
  IfStmt {..} ->
    let not_c = HBinary HEquals (val ifStmtCondition) (HInt 0)
        conds' = ifStmtCondition <| conds
        t = transitionRelation' conds' r ifStmtThen
        e = transitionRelation' conds' r ifStmtElse
    in  HOr $
        HAnd (HNot not_c |:> t) |:>
        HAnd (not_c |:> e)
  ModuleInstance {..} -> not_supported
  Skip {..} -> HAnd mempty
 where
  ufVal :: Maybe Id -> L (Expr Int) -> HornExpr
  ufVal fname = HApp fname . fmap val

  val :: Expr Int -> HornExpr
  val = \case
    Constant {..} -> parseVerilogInt constantValue
    Variable {..} -> HVar { hVarName   = varName
                          , hVarModule = varModuleName
                          , hVarIndex  = exprData
                          , hVarType   = Value
                          , hVarRun    = r
                          }
    UF {..}     -> ufVal (Just ufName) ufArgs
    IfExpr {..} -> ufVal Nothing (ifExprCondition |:> ifExprThen |> ifExprElse)
    Str {..}    -> not_supported
    Select {..} -> ufVal Nothing (selectVar <| selectIndices)

  tagWithCond :: PathCond -> Expr Int -> HornExpr
  tagWithCond es e =
    case es of
      SQ.Empty -> tag e
      _        -> ufTag (es |> e)

  ufTag :: L (Expr Int) -> HornExpr
  ufTag = HOr . fmap tag

  tag :: Expr Int -> HornExpr
  tag = \case
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
    { hornHead   = HBinary HIff
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

type G r = Members '[Reader AF, PE.Error IodineException, Trace] r

type FD r = (G r, Members '[Reader NextVars] r)
type FDM r = (G r, Members '[Reader ModuleSt, Reader NextVars] r)
type FDS r = (G r, Members '[Reader ModuleSt, Reader StmtSt, Reader NextVars] r)

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
      trace $ show ("isTop" :: String, isTop)
      trace $ show ("as" :: String, as)
      trace $ show ("vs" :: String, vs)
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

getUpdatedVariables :: Stmt a -> Ids
getUpdatedVariables = \case
  Block {..}          -> mfold getUpdatedVariables blockStmts
  Assignment {..}     -> HS.singleton $ varName assignmentLhs
  IfStmt {..}         -> mfold getVariables [ifStmtThen, ifStmtElse]
  ModuleInstance {..} -> not_supported
  -- PhiNode {..}        -> mempty
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

toSubsTags :: Id -> HM.HashMap Id Int -> L (HornExpr, HornExpr)
toSubsTags m = HM.foldlWithKey' go mempty
 where
  go subs v n =
    subs
      |> (HVar v m 0 Tag LeftRun,  HVar v m n Tag LeftRun)
      |> (HVar v m 0 Tag RightRun, HVar v m n Tag RightRun)

-- -----------------------------------------------------------------------------
-- other stuff
-- -----------------------------------------------------------------------------

throw :: G r => String -> Sem r a
throw = PE.throw . IE VCGen

dt_trace :: String -> a -> a
dt_trace = DT.trace
