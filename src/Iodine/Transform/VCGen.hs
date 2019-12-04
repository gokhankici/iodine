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
import           Iodine.Transform.Normalize (NormalizeOutput)
import           Iodine.Transform.TransitionRelation
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
import           Polysemy
import qualified Polysemy.Error                as PE
import           Polysemy.Reader
import           Polysemy.State
import qualified Polysemy.Trace                as PT
import           Text.Printf


type Ids = HS.HashSet Id

-- | State relevant to statements
data StmtSt = StmtSt { _currentVariables     :: Ids -- | all vars in this block

                     -- the rest is the filtered version of the Annotations type
                     , _currentSources       :: Ids
                     , _currentSinks         :: Ids
                     , _currentInitialEquals :: Ids
                     , _currentAlwaysEquals  :: Ids
                     , _currentAssertEquals  :: Ids
                     }
              deriving (Show)

makeLenses ''StmtSt


{- |
Verification condition generation creates the following 7 type of horn
constraints to encode our check:

1. Initialize: Encodes that initially, every tag is set to 0. We also encode
that the values of the variables that annotated as `initial_eq` or `always_eq`
are the same. Keep in mind that `always_eq` annotations apply to the rest of the
constraints listed below as well.

2. Tag Reset: The tags of the sources are set to 1, and the tags of the rest of
the variables are set to zero.

3. Source Tag Reset: The tags of the sources are set to 0.

4. Next: Always blocks take a single step, and the variables values are updated.

5. Sink check: Checks that variables defined as sinks are tainted equally.

6. Assert equals check: Checks that variables that are annotated as `assert_eq`
always have the same value.

7. Non-interference checks: This is used to make sure that the invariants of
different always blocks are consistent under interference.
-}
vcgen :: G r => NormalizeOutput -> Sem r VCGenOutput
vcgen (ssaIR, trNextVariables) = runReader (NextVars trNextVariables) $
  do unless (SQ.length ssaIR == 1) $
       throw $ printf "expecting a single module"
     combine vcgenMod ssaIR

vcgenMod :: FD r => Module Int -> Sem r Horns
vcgenMod m@Module {..} = do
  annots <- getAnnotations moduleName

  assert (SQ.null gateStmts) $
    "Gate statements should have been merged into always* blocks"
  assert singleBlockForEvent $
    "There should be at most one always block for each event type"

  combine regularChecks alwaysBlocks
    <||> interferenceChecks allStmts
    <||> combine moduleInstanceChecks moduleInstances
    & runReader m
    & runReader annots

  where
    allStmts = abStmt <$> alwaysBlocks
    allEvents = fmap (const ()) <$> toList (abEvent <$> alwaysBlocks)
    singleBlockForEvent = length allEvents == length (nub allEvents)

regularChecks :: FDM r => AlwaysBlock Int -> Sem r Horns
regularChecks AlwaysBlock{..} =
  withStmt abStmt
  $    return mempty
  ||>  initialize abStmt abEvent
  ||>  tagReset abStmt abEvent
  ||>  srcTagReset abStmt abEvent
  ||>  next abStmt
  <||> sinkCheck abStmt
  <||> assertEqCheck abStmt

moduleInstanceChecks :: FD r => ModuleInstance Int -> Sem r Horns
moduleInstanceChecks _ = notSupportedM


-- -------------------------------------------------------------------------------------------------
initialize :: FDS r => S -> Event Int -> Sem r (Horn ())
-- -------------------------------------------------------------------------------------------------
initialize stmt event = do
  Module {..} <- ask
  -- untag everything
  zeroTags <- foldl' (mkZeroTags moduleName) mempty <$> asks (^. currentVariables)
  -- init_eq and always_eq are initially set to the same values
  initEqs <- HS.union <$> asks (^. currentInitialEquals) <*> asks (^. currentAlwaysEquals)
  let subs = foldl' (valEquals moduleName) zeroTags initEqs
  case event of
    Star -> do
      nextVars <- (IM.! stmtId) <$> asks getNextVars
      let starSubs = toSubs moduleName nextVars
      -- VLT* = False &&
      -- VRT* = False &&
      -- VL_x = VR_x for init_eq(x) and always_eq(x) &&
      -- next ==>
      -- kvar[nextVars]
      return $
        Horn { hornHead   = KVar stmtId starSubs
             , hornBody   = HAnd $ (mkEqual <$> subs) |> transitionRelation stmt
             , hornType   = Init
             , hornStmtId = stmtId
             , hornData   = ()
             }
    _ ->
      -- true ==>
      -- kvar[VLT* = False][VRT* = False][VL_x = VR_x for init_eq(x) or always_eq(x)]
      return $
      Horn { hornHead   = KVar stmtId subs
           , hornBody   = HBool True
           , hornType   = Init
           , hornStmtId = stmtId
           , hornData   = ()
           }
 where
   stmtId = stmtData stmt
   mkZeroTags m subs v =
     subs |> (HVarTL0 v m, HBool False) |> (HVarTR0 v m, HBool False)
   valEquals m subs v =
     subs |> (HVarVL0 v m, HVarVR0 v m)


-- -------------------------------------------------------------------------------------------------
tagReset :: FDS r => S -> Event Int -> Sem r (Horn ())
-- -------------------------------------------------------------------------------------------------
tagReset stmt event = do
  Module {..} <- ask
  srcs        <- asks (^. currentSources)
  vars        <- asks (^. currentVariables)
  let non_srcs = HS.difference vars srcs
  -- sources are tagged
  let srcTagSubs = foldl' (tags moduleName True) mempty srcs
  -- non sources are untagged
  let allTagSubs = foldl' (tags moduleName False) srcTagSubs non_srcs
  case event of
    Star -> do
      aes <- alwaysEqualEqualities stmt
      nextVars  <- (IM.! stmtId) <$> asks getNextVars
      let onlyTagSubs = toSubsTags moduleName nextVars
      let initialTagValues = HAnd $ mkEqual <$> allTagSubs
      return $
        Horn { hornHead   = KVar stmtId onlyTagSubs
             , hornBody   = HAnd $
                            (KVar stmtId mempty <| aes) |>
                            initialTagValues |>
                            transitionRelation stmt
             , hornType   = TagReset
             , hornStmtId = stmtId
             , hornData   = ()
             }
    _ ->
      return $
      Horn { hornHead   = KVar stmtId allTagSubs
           , hornBody   = KVar stmtId mempty
           , hornType   = TagReset
           , hornStmtId = stmtId
           , hornData   = ()
           }
 where
  stmtId = stmtData stmt
  tags m value subs v =
    subs |> (HVarTL0 v m, HBool value) |> (HVarTR0 v m, HBool value)


-- -------------------------------------------------------------------------------------------------
srcTagReset :: FDS r => S -> Event Int -> Sem r (Horn ())
-- -------------------------------------------------------------------------------------------------
srcTagReset stmt event = do
  Module {..} <- ask
  -- sources are untagged
  srcs <- asks (^. currentSources)
  let srcTagSubs = foldl' (mkZeroTags moduleName) mempty srcs
  case event of
    Star -> do
      aes <- alwaysEqualEqualities stmt
      nextVars  <- (IM.! stmtId) <$> asks getNextVars
      let onlyTagSubs = toSubsTags moduleName nextVars
      let initialTagValues = HAnd $ mkEqual <$> srcTagSubs
      return $
        Horn { hornHead   = KVar stmtId onlyTagSubs
             , hornBody   = HAnd $
                            (KVar stmtId mempty <| aes) |>
                            initialTagValues |>
                            transitionRelation stmt
             , hornType   = SourceReset
             , hornStmtId = stmtId
             , hornData   = ()
             }
    _ ->
      return $
      Horn { hornHead   = KVar stmtId srcTagSubs
           , hornBody   = KVar stmtId mempty
           , hornType   = SourceReset
           , hornStmtId = stmtId
           , hornData   = ()
           }

 where
  stmtId = stmtData stmt
  mkZeroTags m subs v =
    subs
      |> (HVar v m 0 Tag LeftRun , HBool False)
      |> (HVar v m 0 Tag RightRun, HBool False)


-- -------------------------------------------------------------------------------------------------
next :: FDS r => S -> Sem r (Horn ())
-- -------------------------------------------------------------------------------------------------
next stmt = do
  Module {..} <- ask
  nextVars <- (IM.! stmtId) <$> asks getNextVars
  aes <- alwaysEqualEqualities stmt
  trace $ show ("equalities" :: String, aes)
  let subs = toSubs moduleName nextVars
  return $ Horn { hornBody   = HAnd $
                               (KVar stmtId mempty <| aes) |>
                               transitionRelation stmt
                , hornHead   = KVar stmtId subs
                , hornType   = Next
                , hornStmtId = stmtId
                , hornData   = ()
                }
  where
    stmtId = stmtData stmt


-- -------------------------------------------------------------------------------------------------
sinkCheck :: FDS r => S -> Sem r Horns
-- -------------------------------------------------------------------------------------------------
sinkCheck stmt = do
  Module {..} <- ask
  snks <- asks (^. currentSinks)
  return $ foldl' (\hs v -> hs |> go moduleName v) mempty snks
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


-- -------------------------------------------------------------------------------------------------
assertEqCheck :: FDS r => S -> Sem r Horns
-- -------------------------------------------------------------------------------------------------
assertEqCheck stmt = do
  Module {..} <- ask
  aes         <- asks (^. currentAssertEquals)
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


-- -------------------------------------------------------------------------------------------------
interferenceChecks :: FDM r => Ss -> Sem r Horns
-- -------------------------------------------------------------------------------------------------
interferenceChecks stmts =
  traverse_ interferenceCheck stmts
  & evalState @ICSts mempty
  & runState @Horns mempty
  & fmap fst

data ICSt = ICSt { icStmt      :: S
                 , writtenVars :: Ids
                 , allVars     :: Ids
                 , aeVars      :: Ids -- | always_eq vars
                 }
type ICSts = IM.IntMap ICSt

interferenceCheck :: (FDM r, Members '[State ICSts, State Horns] r)
                  => S -> Sem r ()
interferenceCheck stmt = do
  -- traverse the statements we have looked at so far
  stmtSt <- computeStmtStM stmt
  let currentSt =
        ICSt { icStmt      = stmt
             , writtenVars = currentWrittenVars
             , allVars     = currentAllVars
             , aeVars      = stmtSt ^. currentAlwaysEquals
             }
  get @ICSts >>=
    traverse_
    (\icSt@ICSt {..} -> do
        -- if the current statement overwrites any variable that the previous
        -- statement uses ...
        when (currentWrittenVars `intersects` allVars) $ do
          h <- interferenceCheckWR currentSt icSt
          modify @Horns (|> h)
        -- if the previous statement overwrites any variable that the current
        -- statement uses ...
        when (writtenVars `intersects` currentAllVars) $ do
          h <- interferenceCheckWR icSt currentSt
          modify @Horns (|> h)
    )
  modify $ IM.insert stmtId currentSt
 where
  stmtId = stmtData stmt
  currentAllVars = getVariables stmt
  currentWrittenVars = getUpdatedVariables stmt

-- return the write/read interference check
interferenceCheckWR :: FDM r
                    => ICSt   -- | statement info that overwrites a variable
                    -> ICSt   -- | statement whose variable is being overwritten
                    -> Sem r (Horn ())
interferenceCheckWR wSt rSt = do
  Module {..} <- ask
  wNext       <- (IM.! stmtData wStmt) <$> asks getNextVars
  let rNext = HM.filterWithKey (\var _ -> HS.member var rVars) wNext
      subs = toSubs moduleName rNext
      rId = stmtData rStmt
      wId = stmtData wStmt
      _rAlwaysEqs = HS.foldl' mkAEs mempty (aeVars wSt `HS.union` aeVars rSt)
      rAlwaysEqs = dt_trace (printf "w:%d, r:%d, %s" wId rId (show _rAlwaysEqs)) _rAlwaysEqs
      mkAEs acc v =
        (case HM.lookup v rNext of
           Just n  -> acc
                      |> (HVar v moduleName n Tag   LeftRun, HVar v moduleName n Tag   RightRun)
                      |> (HVar v moduleName n Value LeftRun, HVar v moduleName n Value RightRun)
           Nothing -> acc)
        |> (HVarTL0 v moduleName, HVarTR0 v moduleName)
        |> (HVarVL0 v moduleName, HVarVR0 v moduleName)
  return $
    Horn { hornHead   = KVar rId subs
         , hornBody   = HAnd
                        $   KVar rId mempty
                        |:> KVar wId mempty
                        |>  HAnd (mkEqual <$> rAlwaysEqs)
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
-- Helper functions
-- -----------------------------------------------------------------------------

alwaysEqualEqualities :: FDS r => S -> Sem r (L HornExpr)
alwaysEqualEqualities stmt = do
  Module {..} <- ask
  nextVars <- (IM.! stmtId) <$> asks getNextVars
  foldl' (go moduleName nextVars) mempty <$> asks (^. currentAlwaysEquals)
  where
    stmtId = stmtData stmt
    go m nvs exprs v =
      let exprs' =
            exprs |>
            HBinary HEquals (HVarVL0 v m) (HVarVR0 v m) |>
            HBinary HIff    (HVarTL0 v m) (HVarTR0 v m)
      in case HM.lookup v nvs of
           Just n  -> exprs' |> HBinary HEquals (HVar v m n Value LeftRun) (HVar v m n Value RightRun)
           Nothing -> exprs'

type S = Stmt Int
type Ss = L S
type Horns = L (Horn ())

type VCGenOutput = Horns

type Substitutions = HM.HashMap Id Int
newtype NextVars = NextVars { getNextVars :: IM.IntMap Substitutions }

type G r = Members '[ Reader AnnotationFile
                    , PE.Error IodineException
                    , PT.Trace
                    ] r

type FD r  = (G r,   Members '[Reader NextVars] r)      -- FD  = global effects + next var map
type FDM r = (FD r,  Members '[Reader (Module Int)] r)  -- FDM = FD + current module
type FDS r = (FDM r, Members '[Reader StmtSt] r)        -- FDS = FDM + current statement state

withStmt :: FDM r => S -> Sem (Reader StmtSt ': r) a -> Sem r a
withStmt s act = do
  stmtSt <- computeStmtStM s
  act & runReader stmtSt

computeStmtStM :: FDM r => S -> Sem r StmtSt
computeStmtStM s = do
  m@Module{..} <- ask
  as  <- getAnnotations moduleName
  return $ computeStmtSt as m s

computeStmtSt :: Annotations -> Module Int -> S -> StmtSt
computeStmtSt as Module{..} stmt =
  StmtSt
  { _currentVariables     = vs
  , _currentSources       = filterAs sources
  , _currentSinks         = filterAs sinks
  , _currentInitialEquals = HS.union extraInitEquals $ filterAs initialEquals
  , _currentAlwaysEquals  = filterAs alwaysEquals
  , _currentAssertEquals  = filterAs assertEquals
  }
  where
    filterAs l = HS.intersection (as ^. l) vs 
    vs = getVariables stmt
    extraInitEquals =
      foldl'
      (\vars -> \case
          w@Wire{..} ->
            if   ((Input w) `SQ.elemIndexL` ports == Nothing)
            then HS.insert variableName vars
            else vars
          Register{..} ->
            vars)
      mempty
      variables

getUpdatedVariables :: Stmt a -> Ids
getUpdatedVariables = \case
  Block {..}          -> mfold getUpdatedVariables blockStmts
  Assignment {..}     -> HS.singleton $ varName assignmentLhs
  IfStmt {..}         -> mfold getVariables [ifStmtThen, ifStmtElse]
  Skip {..}           -> mempty

toSubs :: Id                     -- | module name
       -> Substitutions          -- | substitution map
       -> L (HornExpr, HornExpr) -- | variable updates for the kvar
toSubs m = HM.foldlWithKey' go mempty
 where
  go subs v n =
    subs
      |> (HVar v m 0 Tag   LeftRun,  HVar v m n Tag   LeftRun)
      |> (HVar v m 0 Value LeftRun,  HVar v m n Value LeftRun)
      |> (HVar v m 0 Tag   RightRun, HVar v m n Tag   RightRun)
      |> (HVar v m 0 Value RightRun, HVar v m n Value RightRun)

toSubsTags :: Id                     -- | module name
           -> Substitutions          -- | substitution map
           -> L (HornExpr, HornExpr) -- | variable updates for the kvar (for tags only)
toSubsTags m = HM.foldlWithKey' go mempty
 where
  go subs v n =
    subs
      |> (HVar v m 0 Tag LeftRun,  HVar v m n Tag LeftRun)
      |> (HVar v m 0 Tag RightRun, HVar v m n Tag RightRun)

throw :: G r => String -> Sem r a
throw = PE.throw . IE VCGen

