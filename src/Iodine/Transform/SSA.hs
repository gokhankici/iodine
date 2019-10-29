{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Transform.SSA
  ( Expr(..)
  , ssa
  , SSAIR
  )
where

import           Iodine.Language.IRParser (ParsedIR)
import           Iodine.Language.VerilogIR hiding (Expr)
import qualified Iodine.Language.VerilogIR as VIR
import           Iodine.Language.Types

import           Data.Monoid ((<>))
import           Control.Monad.State.Lazy

import Debug.Trace

type SSAIR = L (Module Stmt Expr ())

data Expr a = VExpr (VIR.Expr a) | PhiNode (L (VIR.Expr a)) a
            deriving (Show)

ssa :: ParsedIR -> SSAIR
ssa modules = fmap ((mapStmt ssaStmt) . (mapExpr VExpr)) modules

ssaStmt :: Stmt Expr a -> Stmt Expr a
ssaStmt stmt = trace (show $ snd <$> stmt') undefined
  where
    (stmt', _n') = runState (traverse act stmt) 0

    act :: a -> S (a, Int)
    act a = do
      n <- get
      put (n+1)
      return (a, n)

instance Functor Expr where
  fmap f (VExpr vexpr) = VExpr (f <$> vexpr)
  fmap f (PhiNode es a) = PhiNode (fmap f <$> es) (f a)

instance Foldable Expr where
  foldMap f (VExpr vexpr) = foldMap f vexpr
  foldMap f (PhiNode es a) = foldMap (foldMap f) es <> f a

instance Traversable Expr where
  traverse m (VExpr vexpr) = VExpr <$> traverse m vexpr
  traverse m (PhiNode es a) = PhiNode <$> traverse (traverse m) es <*> m a

type S = State Int


