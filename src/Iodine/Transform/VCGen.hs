{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Transform.VCGen
  ( vcgen
  , VCGenOutput
  , VCGenError(..)

  , Horn(..)
  , HornId(..)
  , HornType(..)
  , HornExpr(..)
  , HornBinaryOp(..)
  , HornUnaryOp(..)
  )
where

import Iodine.Language.Annotation
import Iodine.Language.Types
import Iodine.Language.IR
import Iodine.Transform.SSA (SSAOutput, SSAIR)

import           Control.Monad
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict      as HM
import qualified Data.IntMap              as IM
import qualified Data.Sequence            as SQ
import           Polysemy
import qualified Polysemy.Error           as PE
import           Polysemy.Reader
import           Polysemy.State
import           Text.Printf

-- -----------------------------------------------------------------------------
-- data types
-- -----------------------------------------------------------------------------

data Horn a = Horn { hornHead :: HornExpr a
                   , hornBody :: HornExpr a
                   , hornType :: HornType
                   , hornId   :: HornId
                   , hornData :: a
                   }

newtype HornId = HornId { getHornId :: Int }

data HornBinaryOp = HAnd | HOr | HEquals | HImplies

data HornUnaryOp = HNot

data HornType = Init
              | TagReset
              | SourceReset
              | Next
              | TagEqual
              | Interference
              | WellFormed

data HornExpr a =
  HApp { hAppFun  :: Id
       , hAppArgs :: L (HornExpr a)
       , hExprId  :: a
       }
  | HVar { hVarName :: Id
         , hExprId  :: a
         }
  | HBinary { hBinaryOp  :: HornBinaryOp
            , hBinaryLhs :: HornExpr a
            , hBinaryRhs :: HornExpr a
            , hExprId    :: a
            }
  | HUnary { hUnaryOp  :: HornUnaryOp
           , hUnaryArg :: HornExpr a
           , hExprId  :: a
           }
  | KVar { hKVarSubs :: HM.HashMap Id (HornExpr a)
         , hExprId   :: a 
         }

-- -----------------------------------------------------------------------------
-- implementation
-- -----------------------------------------------------------------------------

vcgen :: G r => SSAOutput -> Sem r VCGenOutput
vcgen (ssaIR, trNextVariables) = 
  vcgenHelper ssaIR
  & runReader (NextVars trNextVariables)
  & evalState St

vcgenHelper :: FD r => SSAIR -> Sem r VCGenOutput
vcgenHelper ssaIR = do
  unless (SQ.length ssaIR == 1) $
    throw $ printf "expecting a single module"
  combine vcgenMod ssaIR

vcgenMod :: Module Int -> Sem r Hs
vcgenMod Module{..} =
  (SQ.><) <$>
  combine regularCheck allStmts <*>
  interferenceChecks allStmts
  where
    allStmts = gateStmts SQ.>< (abStmt <$> alwaysBlocks)

regularCheck :: S -> Sem r Hs
regularCheck s =
  pure SQ.empty |>
  initialize s |>
  tagReset s |>
  srcReset s |>
  next s
  where
    (|>) = liftM2 (SQ.|>)

-- TODO
initialize :: S -> Sem r H
initialize = undefined

-- TODO
tagReset :: S -> Sem r H
tagReset = undefined

-- TODO
srcReset :: S -> Sem r H
srcReset = undefined

-- TODO
next :: S -> Sem r H
next s = stmtVC s >> undefined

-- TODO
interferenceChecks :: Ss -> Sem r Hs
interferenceChecks _ = interferenceCheck undefined undefined >> return SQ.empty

-- TODO
interferenceCheck :: S -> S -> Sem r H
interferenceCheck _rStmt _wStmt = undefined

-- TODO
exprVC :: Expr Int -> Sem r (HornExpr ())
exprVC _e = undefined

-- TODO
stmtVC :: S -> Sem r (HornExpr ())
stmtVC _s = exprVC undefined >> undefined

-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

type S  = Stmt Int
type Ss = L S
type H  = Horn ()
type Hs = L H

type VCGenOutput = Hs

newtype NextVars = NextVars { _getNextVars :: IM.IntMap (HM.HashMap Id Int) }
type AF = AnnotationFile ()

type G r = Members '[ Reader AF
                    , PE.Error VCGenError
                    ] r

type FD r = ( G r
            , Members '[ Reader NextVars
                       , State St
                       ] r
            )

data St = St

-- -----------------------------------------------------------------------------
-- other stuff
-- -----------------------------------------------------------------------------

newtype VCGenError = VCGenError String
                     deriving (Show)

throw :: G r => String -> Sem r a
throw = PE.throw . VCGenError

combine :: Traversable t => (a -> Sem r (L b)) -> t a -> Sem r (L b)
combine act as = foldl' (SQ.><) SQ.empty <$> traverse act as
