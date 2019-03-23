{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Verylog.Abduction.Transform ( giveUniqueId
                                   , undoUniqueId
                                   ) where
import Verylog.Language.Types
import Verylog.Solver.FP.Types (FPStA(..), FPQualifierA(..))

import           Control.Lens hiding (Index)
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Functor.Product
import           Data.Functor.Compose
import           Data.Hashable
import qualified Data.IntMap.Strict  as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text as T
import           GHC.Generics hiding (S, to)
import qualified Data.Graph.Inductive as Gr


-- -----------------------------------------------------------------------------
-- Type of the state
-- -----------------------------------------------------------------------------
newtype M m k v = M (m k v)

data UniqueIdState a i =
  UniqueIdState { _counter    :: i
                , _idMap      :: M HM.HashMap a i
                -- , _reverseMap :: M im i a
                }
  deriving (Generic)

makeLenses ''UniqueIdState

-- -----------------------------------------------------------------------------
-- Exported Functions
-- -----------------------------------------------------------------------------

-- | Gives a unique id for every variable occurs in the IR.
-- | It also returns the state needed for undoing this operation.
-- | (see @undoUniqueId@)
giveUniqueId :: (UniqueId a, MyIndex i, IRMonad f)
             => f a
             -> f (a, i)
giveUniqueId fa = evalState (runM uid fa) $
                  UniqueIdState { _counter    = initialIndex
                                , _idMap      = genericMap_empty
                                -- , _reverseMap = genericMap_empty
                                }

-- | Undos the @giveUniqueId@ operation.
undoUniqueId :: (UniqueId a, IRMonad f)
             => f (a, i)
             -> f a
undoUniqueId = fmapIRMonad fst


-- -----------------------------------------------------------------------------
-- Used type class and its instances
-- -----------------------------------------------------------------------------

-- | A generic map interface with the bare minimums.
class GenericMap m k v where
  genericMap_empty  :: M m k v
  genericMap_lookup :: k -> M m k v -> Maybe v
  genericMap_insert :: k -> v -> M m k v -> M m k v
  -- genericMap_find   :: k -> M m k v -> v
  -- genericMap_show   :: (Show k, Show v) => M m k v -> String

newtype IM' k v = IM' (IM.IntMap v) -- index map

instance GenericMap IM' Int a where
  genericMap_empty                  = M (IM' IM.empty)
  genericMap_lookup k (M (IM' m))   = IM.lookup k m
  genericMap_insert k v (M (IM' m)) = M (IM' (IM.insert k v m))
  -- genericMap_find k (M (IM' m))     = m IM.! k
  -- genericMap_show (M (IM' m))       = show m

instance (Eq k, Hashable k) => GenericMap HM.HashMap k v where
  genericMap_empty            = M HM.empty
  genericMap_lookup k (M m)   = HM.lookup k m
  genericMap_insert k v (M m) = M (HM.insert k v m)
  -- genericMap_find k (M m)     = m HM.! k
  -- genericMap_show (M m)       = show m


-- -----------------------------------------------------------------------------

class (Ord i, Hashable i) => MyIndex i where
  initialIndex :: i        -- | First element in the series.
  nextIndex    :: i -> i   -- | Return the next element in the ordering.

instance MyIndex Int where
  initialIndex = 0
  nextIndex    = (+ 1)

-- -----------------------------------------------------------------------------

-- | We can create an @MyIndex@ from the given "a".
class (Eq a, Hashable a) => UniqueId a where
  uid :: (MyIndex i) => a -> State (UniqueIdState a i) (a, i)

instance UniqueId T.Text where
  uid a = do
    l <- uses idMap (genericMap_lookup a)
    case l of
      Nothing -> do
        n <- use counter
        idMap      %= genericMap_insert a n
        -- reverseMap %= genericMap_insert n a
        counter    %= nextIndex
        return (a, n)
      Just n ->
        return (a, n)

-- -----------------------------------------------------------------------------

-- | "a" is the type of the variables in f where f is a data type used in our IR.
class IRMonad f where
  runM :: (Monad m, Eq b, Hashable b)
       => (a -> m b)
       -> f a
       -> m (f b)

instance IRMonad EventA where
  runM _ (Star)        = return Star
  runM _ (Assign)      = return Assign
  runM m (PosEdge{..}) = PosEdge <$> m eventVar
  runM m (NegEdge{..}) = NegEdge <$> m eventVar

instance IRMonad BlockMetadataA where
  runM m bm = do
    regs'  <- runM m (bm^.mRegisters)
    wires' <- runM m (bm^.mWires)
    rs'    <- runM m (bm^.mRegReadSet)
    ws'    <- runM m (bm^.mRegWriteSet)
    return $ BlockMetadata { _mRegisters   = regs'
                           , _mWires       = wires'
                           , _mRegReadSet  = rs'
                           , _mRegWriteSet = ws'
                           }

instance IRMonad AnnotStA where
  runM m st = do
    sources'      <- runM m (st^.sources)
    sinks'        <- runM m (st^.sinks)
    taintEq'      <- runM m (st^.taintEq)
    assertEq'     <- runM m (st^.assertEq)
    sanitize'     <- runM m (st^.sanitize)
    sanitizeGlob' <- runM m (st^.sanitizeGlob)
    return $
      AnnotSt { _sources      = sources'
              , _sinks        = sinks'
              , _taintEq      = taintEq'
              , _assertEq     = assertEq'
              , _sanitize     = sanitize'
              , _sanitizeGlob = sanitizeGlob'
              }

instance IRMonad PortA where
  runM m (Input{..})  = Input  <$> m portName
  runM m (Output{..}) = Output <$> m portName

instance IRMonad VarA where
  runM m (Register{..}) = Register <$> m varName
  runM m (Wire{..})     = Wire     <$> m varName

instance IRMonad IRA where
  runM m (Always{..}) =
    Always
    <$> runM m event
    <*> runM m alwaysStmt
    <*> return alwaysLoc

  runM m (ModuleInst{..}) =
    ModuleInst modInstName
    <$> mapM (runM m) modParams
    <*> runM m modInstSt

instance IRMonad StmtA where
  runM _ Skip                  = return Skip
  runM m (Block{..})           = Block <$> mapM (runM m) blockStmts
  runM m (BlockingAsgn{..})    = BlockingAsgn <$> m lhs <*> runM m rhs
  runM m (NonBlockingAsgn{..}) = NonBlockingAsgn <$> m lhs <*> runM m rhs
  runM m (IfStmt{..})          = IfStmt <$> runM m ifCond
                                        <*> runM m thenStmt
                                        <*> runM m elseStmt

instance IRMonad StA where
  runM m st = St
              <$> mapM (runM m) (st^.ports)
              <*> mapM (runM m) (st^.irs)

instance IRMonad VExprA where
  runM m (VVar{..}) = VVar <$> m vVarName
  runM m (VUF{..})  = do
    n'  <- m vVarName
    fn' <- m vFuncName
    as' <- mapM (runM m) vFuncArgs
    return $ VUF { vVarName  = n'
                 , vFuncName = fn'
                 , vFuncArgs = as'
                 }

instance IRMonad AlwaysBlockA where
  runM m (AB{..}) = do
    e'  <- runM m _aEvent
    s'  <- runM m _aStmt
    st' <- runM m _aSt
    md' <- runM m _aMd
    return $ AB { _aEvent = e'
                , _aStmt  = s'
                , _aSt    = st'
                , _aMd    = md'
                , ..
                }

instance IRMonad HS.HashSet where
  runM m s = foldl' go (return HS.empty) s
    where
      go act a = do
        s' <- act
        a' <- m a
        return $ HS.insert a' s'

instance IRMonad FPStA where
  runM m (FPSt{..}) = do
    as'     <- mapM (runM m) _fpABs
    annots' <- runM m _fpAnnotations
    return $
      FPSt { _fpABs         = as'
           , _fpAnnotations = annots'
           , ..
           }

instance (IRMonad f, Traversable t) => IRMonad (Compose t f) where
  runM m (Compose ts) = Compose <$> (mapM (runM m) ts)

instance (IRMonad f1, IRMonad f2) => IRMonad (Product f1 f2) where
  runM m (Pair f1a f2a) = Pair <$> runM m f1a <*> runM m f2a

instance IRMonad FPQualifierA where
  runM m (QualifImp{..}) = do
    l  <- m qualifLhs
    rs <- mapM m qualifRhss
    return $ QualifImp { qualifLhs  = l
                       , qualifRhss = rs
                       }
  runM m (QualifIff{..}) = do
    l  <- m qualifLhs
    rs <- mapM m qualifRhss
    return $ QualifIff { qualifLhs  = l
                       , qualifRhss = rs
                       }
  runM m (QualifPairs{..}) = QualifPairs <$> (mapM m qualifEqs)
  runM m (QualifAssume{..}) = QualifAssume <$> (mapM m qualifAssume)

newtype IRGr gr b a = IRGr (gr a b)

instance (Gr.DynGraph gr) => IRMonad (IRGr gr b) where
  runM m (IRGr g) = do
    nodes' <- mapM (\(n,a) -> fmap ((,) n) (m a)) nodes
    return $ IRGr $ Gr.mkGraph nodes' edges
    where
      edges  = Gr.labEdges g
      nodes  = Gr.labNodes g

fmapIRMonad :: (IRMonad f, Hashable b, Eq b) => (a -> b) -> f a -> f b
fmapIRMonad f = runIdentity . runM (return . f)
