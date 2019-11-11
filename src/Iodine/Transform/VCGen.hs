{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Transform.VCGen
  ( vcgen
  , VCGenOutput
  , VCGenError(..)
  )
where

import Iodine.Language.Annotation
import Iodine.Language.Types
import Iodine.Language.IR
import Iodine.Transform.SSA (SSAOutput, SSAIR)
import Iodine.Transform.Horn

import           Control.Monad
import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.IntMap              as IM
import           Data.Maybe
import qualified Data.Sequence            as SQ
import           Polysemy
import qualified Polysemy.Error           as PE
import           Polysemy.Reader
import           Polysemy.State
import           Text.Printf

-- -----------------------------------------------------------------------------
-- vcgen state
-- -----------------------------------------------------------------------------

type Ids = HS.HashSet Id

data St = St { _currentModule    :: Maybe (Module Int)
             , _isTopModule      :: Bool

             , _currentSources   :: Ids
             , _currentSinks     :: Ids
             , _currentInitEqs   :: Ids
             , _currentAlwaysEqs :: Ids
             , _currentVariables :: Ids
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
                  pure SQ.empty ||> initialize s ||> tagReset s ||> srcReset s ||> next s

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


tagReset :: FD r => S -> Sem r (Horn ())
tagReset stmt = do
  srcs <- gets (^. currentSources)
  vars <- gets (^. currentVariables)
  let non_srcs = HS.difference vars srcs
  let subs1 = foldl' (tags True) mempty srcs     -- sources are tagged
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
      let n = m HM.! v
      in exprs |>
         HBinary HEquals (HVar v 0 Value LeftRun) (HVar v 0 Value RightRun) |>
         HBinary HEquals (HVar v n Value LeftRun) (HVar v n Value RightRun)

-- TODO
interferenceChecks :: Ss -> Sem r Horns
interferenceChecks _ = interferenceCheck undefined undefined >> return SQ.empty

-- TODO
interferenceCheck :: S -> S -> Sem r (Horn ())
interferenceCheck _rStmt _wStmt = undefined

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
      Constant{..} -> undefined
      Variable{..} -> HVar { hVarName  = varName
                           , hVarIndex = exprData
                           , hVarType  = Value
                           , hVarRun   = r
                           }
      UF{..}       -> ufVal ufArgs
      IfExpr{..}   -> ufVal (ifExprCondition |:> ifExprThen |> ifExprElse)
      Str{..}      -> undefined
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

type FD r = ( G r
            , Members '[ Reader NextVars
                       , State St
                       ] r
            )

initialState :: St
initialState = St { _currentModule    = Nothing
                  , _isTopModule      = False

                  , _currentSources   = mempty
                  , _currentSinks     = mempty
                  , _currentInitEqs   = mempty
                  , _currentAlwaysEqs = mempty
                  , _currentVariables = mempty
                  }

infixl 9 ||>
(||>) :: Applicative f => f (L a) -> f a -> f (L a)
(||>) fas fa = (|>) <$> fas <*> fa

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
  where
    vs = getVariables stmt

unsetAnnotations :: FD r => Sem r ()
unsetAnnotations = do
  modify $ currentVariables .~ mempty
  modify $ currentSources   .~ mempty
  modify $ currentSinks     .~ mempty
  modify $ currentInitEqs   .~ mempty
  modify $ currentAlwaysEqs .~ mempty

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

combine :: Traversable t => (a -> Sem r (L b)) -> t a -> Sem r (L b)
combine act as = foldl' (SQ.><) SQ.empty <$> traverse act as

mfold :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mfold f as = foldl' (\ms a -> f a <> ms) mempty as
