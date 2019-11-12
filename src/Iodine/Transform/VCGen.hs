{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Transform.VCGen
  ( vcgen
  , VCGenOutput
  , VCGenError(..)
  )
where

import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Language.Types
import           Iodine.Transform.Horn
import           Iodine.Transform.SSA       (SSAIR, SSAOutput)

import           Control.Lens
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.IntMap                as IM
import           Data.Maybe
import qualified Data.Sequence              as SQ
import qualified Data.Text                  as T
import           Polysemy
import qualified Polysemy.Error             as PE
import           Polysemy.Reader
import           Polysemy.State
import           Text.Printf
import           Text.Read (readEither)

-- -----------------------------------------------------------------------------
-- vcgen state
-- -----------------------------------------------------------------------------

type Ids = HS.HashSet Id

data St = St { _currentModule    :: Maybe (Module Int) -- current module
             , _isTopModule      :: Bool -- is the current module the top module?

             -- the following are block/stmt level state
             , _currentVariables :: Ids -- all vars in this block
             , _currentSources   :: Ids -- sources in this block
             , _currentSinks     :: Ids -- sinks in this block
             , _currentInitEqs   :: Ids -- init_eq vars in this block
             , _currentAlwaysEqs :: Ids -- always_eq vars in this block
             , _currentAssertEqs :: Ids -- assert_eq vars in this block
             }

makeLenses ''St

-- -----------------------------------------------------------------------------
-- vcgen implementation
-- -----------------------------------------------------------------------------

vcgen :: G r => SSAOutput -> Sem r VCGenOutput
vcgen (ssaIR, trNextVariables) =
  vcgenHelper ssaIR
  & runReader (NextVars trNextVariables)
  & evalState initialState

vcgenHelper :: FD r => SSAIR -> Sem r VCGenOutput
vcgenHelper ssaIR = do
  unless (SQ.length ssaIR == 1) $
    throw $ printf "expecting a single module"
  combine vcgenMod ssaIR

vcgenMod :: FD r => Module Int -> Sem r Horns
vcgenMod Module{..} = withModule Module{..} $
  (SQ.><) <$>
  combine regularChecks allStmts <*>
  interferenceChecks allStmts
  where
    allStmts = gateStmts SQ.>< (abStmt <$> alwaysBlocks)


regularChecks :: FD r => S -> Sem r Horns
regularChecks s = withStmt s $
                  pure SQ.empty ||> initialize s ||> tagReset s ||> srcReset s ||> next s <||> assertEqCheck s


-- -----------------------------------------------------------------------------
-- 1. initialize
-- -----------------------------------------------------------------------------
-- Initially, all variables are untagged. We assume that init_eq or
-- always_eq variables are equal to each other in two runs.
-- -----------------------------------------------------------------------------

initialize :: FD r => S -> Sem r (Horn ())
initialize stmt = do
  subs1 <- foldl' zeroTags mempty <$> gets (^. currentVariables) -- untag everything
  subs2 <- foldl' valEquals subs1 <$> gets (^. currentInitEqs)   -- init_eq vars are equal
  subs  <- foldl' valEquals subs2 <$> gets (^. currentAlwaysEqs) -- always_eq vars are equal
  return $ Horn { hornHead = HAnd mempty
                , hornBody = KVar stmtId subs
                , hornType = Init
                , hornData = ()
                }
  where
    stmtId = stmtData stmt
    zeroTags subs v =
      subs |>
      (HVar v 0 Tag LeftRun, HBool False) |>
      (HVar v 0 Tag RightRun, HBool False)
    valEquals subs v =
      subs |>
      (HVar v 0 Value LeftRun, HVar v 0 Value RightRun)


-- -----------------------------------------------------------------------------
-- 2. tag reset
-- -----------------------------------------------------------------------------
-- We set the tags of the source variables, and unset the tags of everything else.
-- -----------------------------------------------------------------------------

tagReset :: FD r => S -> Sem r (Horn ())
tagReset stmt = do
  srcs <- gets (^. currentSources)
  vars <- gets (^. currentVariables)
  let non_srcs = HS.difference vars srcs
  let subs1 = foldl' (tags True) mempty srcs    -- sources are tagged
  let subs = foldl' (tags False) subs1 non_srcs -- non sources are untagged
  return $ Horn { hornHead = KVar stmtId subs
                , hornBody = KVar stmtId mempty
                , hornType = TagReset
                , hornData = ()
                }

  where
    stmtId = stmtData stmt
    tags value subs v = subs |>
                        (HVar v 0 Tag LeftRun, HBool value) |>
                        (HVar v 0 Tag RightRun, HBool value)


-- -----------------------------------------------------------------------------
-- 3. src reset
-- -----------------------------------------------------------------------------
-- We unset the tags of the source variables.
-- -----------------------------------------------------------------------------

srcReset :: FD r => S -> Sem r (Horn ())
srcReset stmt = do
  subs <- foldl' (tags False) mempty <$> gets (^. currentSources) -- sources are untagged
  return $ Horn { hornHead = KVar stmtId subs
                , hornBody = KVar stmtId mempty
                , hornType = SourceReset
                , hornData = ()
                }

  where
    stmtId = stmtData stmt
    tags value subs v = subs |>
                        (HVar v 0 Tag LeftRun, HBool value) |>
                        (HVar v 0 Tag RightRun, HBool value)


-- -----------------------------------------------------------------------------
-- 4. transition relation
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

next :: FD r => S -> Sem r (Horn ())
next stmt = do
  nextVars <- (IM.! stmtId) <$> asks getNextVars
  equalities <- foldl' (ae nextVars) mempty <$> gets (^. currentAlwaysEqs)
  let subs = toSubs nextVars
  return $ Horn { hornBody = HAnd $ (KVar stmtId mempty |:> tr) <> equalities
                , hornHead = KVar stmtId subs
                , hornType = Next
                , hornData = ()
                }
  where
    stmtId = stmtData stmt
    tr = transitionRelation stmt
    ae m exprs v =
      case HM.lookup v m of
        Just n -> exprs |>
                  HBinary HEquals (HVar v 0 Value LeftRun) (HVar v 0 Value RightRun) |>
                  HBinary HEquals (HVar v n Value LeftRun) (HVar v n Value RightRun)
        Nothing -> exprs

transitionRelation ::  S -> HornExpr
transitionRelation s = HAnd $ transitionRelation' LeftRun s |:> transitionRelation' RightRun s

-- TODO
transitionRelation' :: HornVarRun -> S -> HornExpr
transitionRelation' r = \case
  Block{..}          -> HAnd $ transitionRelation' r <$> blockStmts
  Assignment{..}     -> HAnd $
                        HBinary HEquals (valE assignmentLhs) (valE assignmentRhs) |:>
                        HBinary HEquals (tagE assignmentLhs) (tagE assignmentRhs)
  IfStmt{..}         -> let c = valE ifStmtCondition
                            t = transitionRelation' r ifStmtThen
                            e = transitionRelation' r ifStmtElse
                        in HOr $ HBinary HImplies c t |:> HBinary HImplies (HNot c) e
  ModuleInstance{..} -> error "submodules are not supported"
  PhiNode{..}        -> let lhsValue = valE phiLhs
                            lhsTag   = tagE phiLhs
                        in HAnd $
                           HOr ((HBinary HEquals lhsValue . valE) <$> phiRhs) |:>
                           HOr ((HBinary HEquals lhsTag   . tagE) <$> phiRhs)
  Skip{..}           -> HAnd mempty
  where
    ufVal :: L (Expr Int) -> HornExpr
    ufVal = HApp . fmap valE

    ufTag :: L (Expr Int) -> HornExpr
    ufTag = HOr . fmap tagE

    valE :: Expr Int -> HornExpr
    valE = \case
      Constant{..} -> parseVerilogInt constantValue
      Variable{..} -> HVar { hVarName  = varName
                           , hVarIndex = exprData
                           , hVarType  = Value
                           , hVarRun   = r
                           }
      UF{..}       -> ufVal ufArgs
      IfExpr{..}   -> ufVal (ifExprCondition |:> ifExprThen |> ifExprElse)
      Str{..}      -> error "Strings are not handled (yet)"
      Select{..}   -> ufVal (selectVar <| selectIndices)

    tagE :: Expr Int -> HornExpr
    tagE = \case
      Constant{..} -> HBool False
      Variable{..} -> HVar { hVarName  = varName
                           , hVarIndex = exprData
                           , hVarType  = Tag
                           , hVarRun   = r
                           }
      UF{..}       -> ufTag ufArgs
      IfExpr{..}   -> ufTag (ifExprCondition |:> ifExprThen |> ifExprElse)
      Str{..}      -> HBool False
      Select{..}   -> ufTag (selectVar <| selectIndices)

parseVerilogInt :: Id -> HornExpr
parseVerilogInt value =
  case readEither v' of
    Left _   -> HConstant value
    Right n  -> HInt n
  where
    v = T.unpack value
    v' = case v of
           '0' : 'b' : rst -> rst
           _ -> v

-- -----------------------------------------------------------------------------
-- 6. interference checks
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

assertEqCheck :: FD r => S -> Sem r Horns
assertEqCheck stmt = do
  aes <- gets (^. currentAssertEqs)
  return $ foldl' (\hs v -> hs |> go v) mempty aes
  where
    stmtId = stmtData stmt
    go v = Horn { hornHead = HBinary HEquals
                             (HVar v 0 Value LeftRun)
                             (HVar v 0 Value RightRun)
                , hornBody = KVar stmtId mempty
                , hornType = AssertEqCheck
                , hornData = ()
                }



-- -----------------------------------------------------------------------------
-- 6. interference checks
-- -----------------------------------------------------------------------------
-- Always block takes a single step. The variables are updated, obeying the
-- always_eq annotations.
-- -----------------------------------------------------------------------------

data ICSt = ICSt { icStmt :: S,  writtenVars :: Ids, allVars :: Ids }
type ICSts = IM.IntMap ICSt

interferenceChecks :: FD' r => Ss -> Sem r Horns
interferenceChecks stmts =
  (traverse_ interferenceCheck stmts >> get @Horns)
  & evalState @ICSts mempty
  & evalState @Horns mempty

-- TODO
interferenceCheck :: (FD' r,
                      Members '[ State ICSts
                               , State Horns
                               ] r)
                  => S -> Sem r ()
interferenceCheck stmt = do
  -- get the statements we have looked at so far
  icSts <- get @ICSts
  traverse_ (\icSt@ICSt{..} -> do
    when (currentWrittenVars `intersects` allVars) $ do
      h <- interferenceCheckWR currentSt icSt
      modify @Horns (|> h)
    when (writtenVars `intersects` currentAllVars) $ do
      h <- interferenceCheckWR icSt currentSt
      modify @Horns (|> h)
    ) icSts
  modify $ IM.insert stmtId currentSt
  where
    stmtId             = stmtData stmt
    currentSt          = ICSt{ icStmt = stmt
                             , writtenVars = currentWrittenVars
                             , allVars = currentAllVars }
    currentAllVars     = getVariables stmt
    currentWrittenVars = getUpdatedVariables stmt

-- return the write/read interference check
interferenceCheckWR :: FD' r => ICSt -> ICSt -> Sem r (Horn ())
interferenceCheckWR wSt rSt = do
  wNext <- (IM.! stmtData wStmt) <$> asks getNextVars
  let subs = toSubs $ HM.filterWithKey (\var _ -> HS.member var rVars) wNext
      rId  = stmtData rStmt
      wId  = stmtData wStmt
  return $ Horn { hornHead = KVar rId subs
                , hornBody = HAnd $
                             KVar rId mempty |:>
                             KVar wId mempty |>
                             transitionRelation wStmt
                , hornType = Interference
                , hornData = ()
                }
  where
    rStmt = icStmt rSt
    wStmt = icStmt wSt
    rVars = allVars rSt


-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

type S  = Stmt Int
type Ss = L S
type Horns = L (Horn ())

type VCGenOutput = Horns

newtype NextVars = NextVars { getNextVars :: IM.IntMap (HM.HashMap Id Int) }
type AF = AnnotationFile ()

type G r = Members '[ Reader AF
                    , PE.Error VCGenError
                    ] r

type FD' r = (G r, Member (Reader NextVars) r)
type FD r  = (FD' r, Member (State St) r)

initialState :: St
initialState = St { _currentModule    = Nothing
                  , _isTopModule      = False

                  , _currentVariables = mempty
                  , _currentSources   = mempty
                  , _currentSinks     = mempty
                  , _currentInitEqs   = mempty
                  , _currentAlwaysEqs = mempty
                  , _currentAssertEqs = mempty
                  }

infixl 9 ||>
(||>) :: Applicative f => f (L a) -> f a -> f (L a)
(||>) fas fa = (|>) <$> fas <*> fa

infixl 9 <||>
(<||>) :: Applicative f => f (L a) -> f (L a) -> f (L a)
(<||>) = liftA2 (<>)

(|:>) :: (Snoc s s a a, Monoid s) => a -> a -> s
(|:>) a1 a2 = mempty |> a1 |> a2

withModule :: FD r => Module Int -> Sem r a -> Sem r a
withModule m act =
  modify (currentModule .~ Just m) *>
  act <*
  modify (currentModule .~ Nothing)

withStmt :: FD r => S -> Sem r a -> Sem r a
withStmt s act = setAnnotations s *> act <* unsetAnnotations

setAnnotations :: FD r => S -> Sem r ()
setAnnotations stmt = do
  as <- asks afAnnotations
  topModuleName <- asks afTopModule

  Module{..} <- gets (^. currentModule . to fromJust)
  let isTop = moduleName == topModuleName
  modify $ isTopModule .~ isTop

  modify $ currentVariables .~ vs

  let addIf v setter = when (HS.member v vs) $ modify setter
  (flip traverse_) as $ \case
    Source s _        -> when isTop $ addIf s (currentSources %~ HS.insert s)
    Sink s _          -> when isTop $ addIf s (currentSinks   %~ HS.insert s)
    Sanitize ss _     -> when isTop $ traverse_ (\s -> addIf s (currentInitEqs %~ HS.insert s)) ss
    SanitizeMod m v _ -> when (moduleName == m) $ addIf v (currentInitEqs %~ HS.insert v)
    SanitizeGlob s _  -> when isTop $ addIf s (currentAlwaysEqs %~ HS.insert s)
    AssertEq v _      -> when isTop $ addIf v (currentAssertEqs %~ HS.insert v)
  where
    vs = getVariables stmt

unsetAnnotations :: FD r => Sem r ()
unsetAnnotations = do
  modify $ currentVariables .~ mempty
  modify $ currentSources   .~ mempty
  modify $ currentSinks     .~ mempty
  modify $ currentInitEqs   .~ mempty
  modify $ currentAlwaysEqs .~ mempty
  modify $ currentAssertEqs .~ mempty

getVariables :: Stmt a -> Ids
getVariables = \case
  Block{..}          -> mfold getVariables blockStmts
  Assignment{..}     -> mfold go [assignmentLhs, assignmentRhs]
  IfStmt{..}         -> go ifStmtCondition <> mfold getVariables [ifStmtThen, ifStmtElse]
  ModuleInstance{..} -> mempty
  PhiNode{..}        -> mempty
  Skip{..}           -> mempty
  where
    go :: Expr a -> Ids
    go Variable{..} = HS.singleton varName
    go Constant{..} = mempty
    go UF{..}       = mfold go ufArgs
    go IfExpr{..}   = mfold go [ifExprCondition, ifExprThen, ifExprElse]
    go Str{..}      = mempty
    go Select{..}   = go selectVar <> mfold go selectIndices

getUpdatedVariables :: Stmt a -> Ids
getUpdatedVariables = \case
  Block{..}          -> mfold getUpdatedVariables blockStmts
  Assignment{..}     -> HS.singleton $ varName assignmentLhs
  IfStmt{..}         -> mfold getVariables [ifStmtThen, ifStmtElse]
  ModuleInstance{..} -> mempty
  PhiNode{..}        -> mempty
  Skip{..}           -> mempty

toSubs :: HM.HashMap Id Int -> L (HornExpr, HornExpr)
toSubs = HM.foldlWithKey' go mempty
  where
    go subs v n = subs |>
                  (HVar v 0 Tag   LeftRun,  HVar v n Tag   LeftRun) |>
                  (HVar v 0 Value LeftRun,  HVar v n Value LeftRun) |>
                  (HVar v 0 Tag   RightRun, HVar v n Tag   RightRun) |>
                  (HVar v 0 Value RightRun, HVar v n Value RightRun)

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
mfold f as = foldl' (\ms a -> f a <> ms) mempty as

intersects :: HS.HashSet Id -> HS.HashSet Id -> Bool
intersects s1 s2 = go (HS.toList s1)
  where
    go []     = False
    go (a:as) = if HS.member a s2 then True else go as

