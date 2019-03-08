{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction (abduction) where

import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Transform.Utils

import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Hashable
import           Data.List
import           Text.Printf

--------------------------------------------------------------------------------
abduction :: FC.Config -> FPSt -> IO (Bool, FT.FixSolution)
--------------------------------------------------------------------------------
abduction cfg fpst = do
  (isSafe, sol) <- solve cfg fpst
  let r = toR sol
  putStrLn $ show r
  return (isSafe, sol)

-- -----------------------------------------------------------------------------
-- Parsing liquid-fixpoint output
-- -----------------------------------------------------------------------------

data R = TagEq    { varName :: String }
       | ValueEq  { varName :: String }
       | TagEq2   { varName :: String, var2Name :: String }
       | ValueEq2 { varName :: String, var2Name :: String }
       | NoTaint  { varName :: String }
       deriving (Eq)

type RS = HS.HashSet R

toR :: FT.FixSolution -> RS
toR sol = goTops $ HM.elems sol
  where
    goTops :: [FT.Expr] -> RS
    goTops = foldl' (\s e -> s `HS.union` goTop e) mempty

    --------------------------------------------------------------
    goTop :: FT.Expr -> RS
    --------------------------------------------------------------
    goTop (FT.PAnd es) = goTops es
    goTop e            = HS.singleton $ go e

    --------------------------------------------------------------
    go :: FT.Expr -> R
    --------------------------------------------------------------
    go e@(FT.PIff (FT.EVar s1) (FT.EVar s2)) =
      if | not diffRun -> err e -- one must be L and the other R
         | not bothTag -> err e -- both must be tags
         | sameVar     -> TagEq v1
         | not sameVar -> TagEq2 v1 v2
      where
        diffRun    = dr f1 f2
        sameVar    = v1 == v2
        bothTag    = taggedVar f1 && taggedVar f2
        (f1, v1)   = parseVarName $ FT.symbolSafeString s1
        (f2, v2)   = parseVarName $ FT.symbolSafeString s2

    go e@(FT.PIff (FT.EVar s1) e2) =
      if taggedVar f1 && not b then NoTaint v1 else err e
      where
        b        = toBool e2
        (f1, v1) = parseVarName $ FT.symbolSafeString s1

    go (FT.PIff e1 (FT.EVar s2)) =
      go (FT.PIff (FT.EVar s2) e1)

    go e@(FT.PAtom FT.Eq (FT.EVar s1) (FT.EVar s2)) =
      if | not diffRun -> err e -- one must be L and the other R
         | not bothVar -> err e
         | sameVar     -> ValueEq v1
         | not sameVar -> ValueEq2 v1 v2
      where
        bothVar  = not $ taggedVar f1 || taggedVar f2
        diffRun  = dr f1 f2
        sameVar  = v1 == v2
        (f1, v1) = parseVarName $ FT.symbolSafeString s1
        (f2, v2) = parseVarName $ FT.symbolSafeString s2


    go e = err e

    -- one of f1 & f2 is leftVar and the other is rightVar
    dr f1 f2 = (leftVar f1 /= leftVar f2) && (rightVar f1 /= rightVar f2)

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

instance Hashable R where
  hashWithSalt n (TagEq v1)       = hashWithSalt n ("te", v1)
  hashWithSalt n (ValueEq v1)     = hashWithSalt n ("ve", v1)
  hashWithSalt n (TagEq2 v1 v2)   = hashWithSalt n ("te2", v1, v2)
  hashWithSalt n (ValueEq2 v1 v2) = hashWithSalt n ("te2", v1, v2)
  hashWithSalt n (NoTaint v1)     = hashWithSalt n ("nt", v1)
