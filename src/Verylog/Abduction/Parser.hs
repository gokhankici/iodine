{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Parser ( toR
                                , readAnnots
                                , writeAnnots
                                ) where

import Verylog.Abduction.Types

import Verylog.Transform.Utils
import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Verylog.Utils

import qualified Language.Fixpoint.Types as FT

import           Control.Lens           hiding ((<.>))
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           System.FilePath.Posix
import           System.Directory
import           Text.Printf

-- | @.annot.good@ file is concatanated to the annotations defined in the actual file
--   @.annot.bad@ file is used as it is
readAnnots :: FilePath -> M ()
readAnnots f = do
  liftIO $ printf "Reading from annot files:\n%s\n" annotFile
  c <- liftIO $ doesFileExist annotFile
  if c
    then do af <- decodeAnnotStFile annotFile
            assign negAnnots (af^.badAnnot)
            badStDiff (af^.goodAnnot) >>=
              modifying (fpst.fpAnnotations) . mappend
    else return ()
  where
    annotFile = takeDirectory f </> "" <.> takeFileName f <.> "annot"

-- | Writes the current annotations to the files
writeAnnots :: FilePath -> M ()
writeAnnots f = do
  ga <- use (fpst.fpAnnotations)
  ba <- use negAnnots
  let af = mempty & goodAnnot .~ ga & badAnnot .~ ba
  liftIO2 encodeAnnotStFile annotFile af
  where
    annotFile = takeDirectory f </> "" <.> takeFileName f <.> "annot"

type RS = HS.HashSet R

toR :: Sol -> RS
toR sol = goTops $ HM.elems sol
  where
    goTops :: [FT.Expr] -> RS
    goTops = mconcat . fmap goTop

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
        diffRun  = dr f1 f2
        sameVar  = v1 == v2
        bothTag  = taggedVar f1 && taggedVar f2
        (f1, v1) = parseVarName . str2Id $ FT.symbolSafeString s1
        (f2, v2) = parseVarName . str2Id $ FT.symbolSafeString s2

    go e@(FT.PIff (FT.EVar s1) e2) =
      if taggedVar f1 && not b then NoTaint v1 else err e
      where
        b        = toBool e2
        (f1, v1) = parseVarName . str2Id $ FT.symbolSafeString s1

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
        (f1, v1) = parseVarName . str2Id $ FT.symbolSafeString s1
        (f2, v2) = parseVarName . str2Id $ FT.symbolSafeString s2

    go e = err e

    -- one of f1 & f2 is leftVar and the other is rightVar
    dr f1 f2 = (leftVar f1 /= leftVar f2) && (rightVar f1 /= rightVar f2)

    toBool :: FT.Expr -> Bool
    toBool (FT.POr []) = False
    toBool e           = err e

    err :: Show a => a -> b
    err = error . printf "cannot parse %s" . show
