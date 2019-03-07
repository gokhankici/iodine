{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction (abduction) where

-- import Verylog.Solver.Common
import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Transform.Utils

import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import qualified Data.HashMap.Strict as HM
import           Text.Printf

-- import Control.Monad
-- import System.Exit

data R = TagEq    { varName :: String }
       | ValueEq  { varName :: String }
       | TagEq2   { varName :: String, var2Name :: String }
       | ValueEq2 { varName :: String, var2Name :: String }
       | NoTaint  { varName :: String }

abduction :: FC.Config -> FPSt -> IO Bool
--------------------------------------------------------------------------------
abduction cfg fpst = do
  (rc, res) <- solve cfg fpst
  putStrLn $ show res
  putStrLn $ show $ toExpr res
  return False

toExpr :: FT.FixSolution -> [R]
toExpr sol = exprs >>= gos
  where
    exprs = HM.elems sol

    gos :: FT.Expr -> [R]
    gos (FT.PAnd es) = es >>= gos
    gos e            = [go e]

    go :: FT.Expr -> R
    go e@(FT.PIff (FT.EVar s1) (FT.EVar s2)) =
      if | not (lr f1 f2)         -> err e
         | same     && bothTag    -> TagEq   v1
         | same     && bothNotTag -> ValueEq v1
         | not same && bothTag    -> TagEq2   v1 v2
         | not same && bothNotTag -> ValueEq2 v1 v2
         | otherwise              -> err e
      where
        same       = v1 == v2
        bothTag    = taggedVar f1 && taggedVar f2
        bothNotTag = not $ taggedVar f1 || taggedVar f2
        (f1, v1)   = parseVarName $ FT.symbolSafeString s1
        (f2, v2)   = parseVarName $ FT.symbolSafeString s2
    go e@(FT.PIff (FT.EVar s1) e2) =
      if taggedVar f1 && not b then NoTaint v1 else err e
      where
        b        = toBool e2
        (f1, v1) = parseVarName $ FT.symbolSafeString s1
    go (FT.PIff e1 (FT.EVar s2)) = go (FT.PIff (FT.EVar s2) e1)
    go e = err e

    -- one of f1 & f2 is leftVar and the other is rightVar
    lr f1 f2 = (leftVar f1 `xor` leftVar f2) && (rightVar f1 `xor` rightVar f2)
      where xor = (/=) :: Bool -> Bool -> Bool

    toBool :: FT.Expr -> Bool
    toBool (FT.POr []) = False
    toBool e           = err e

    err :: Show a => a -> b
    err = error . printf "cannot parse %s" . show

instance Show R where
  show (TagEq    {..}) = printf "tag_eq(%s)" varName
  show (ValueEq  {..}) = printf "val_eq(%s)" varName
  show (TagEq2   {..}) = printf "tag_eq(%s, %s)" varName var2Name
  show (ValueEq2 {..}) = printf "val_eq(%s, %s)" varName var2Name
  show (NoTaint  {..}) = printf "no_taint(%s)" varName
