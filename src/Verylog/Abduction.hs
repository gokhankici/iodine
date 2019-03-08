{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction (abduction) where

import Prelude hiding (break)

import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Transform.Utils
import Verylog.Language.Types
import Verylog.Utils

import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Hashable
import           System.Random
import           Text.Printf

import Debug.Trace

type Sol = FT.FixSolution
type M   = StateT S IO

-- state used in the annotation algorithm
data S = S { _t       :: Double -- temperature
           , _tMin    :: Double -- minimum temperature
           , _alpha   :: Double -- temperature update factor
           , _step    :: Int    -- # steps taken in each temperature
           , _curStep :: Int    -- current step (between 0 and step)

           , _solution :: Sol     -- solution returned by liquid-fixpoint
           , _cost     :: Double  -- the cost of the current annotations & solution
           , _fpst     :: FPSt    -- current solution

           , _cfg  :: FC.Config
           }

makeLenses ''S

--------------------------------------------------------------------------------
abduction :: FC.Config -> FPSt -> IO (Bool, Sol)
--------------------------------------------------------------------------------
abduction fcConfig st = do
  (_, sol) <- solve fcConfig st
  let r = toR sol
  putStrLn $ show r

  let initialState =
        S { _t        = 1.0
          , _tMin     = 1e-5
          , _alpha    = 0.1
          , _step     = 2
          , _curStep  = 0
          , _solution = sol
          , _cost     = calculateCost (st ^. fpAnnotations) sol
          , _fpst     = st
          , _cfg      = fcConfig
          }

  (isSafe', s') <- runStateT outerLoop initialState
  let sol' = s' ^. solution

  return (isSafe', sol')

calculateCost :: AnnotSt -> Sol -> Double
calculateCost _ _ = 0.0

outerLoop :: M Bool
outerLoop = while False ((>) <$> use t <*> use tMin) $ do
  debugM (printf "%s\nouterloop: t = %g\n" (unlines $ cp 3 l) <$> use t) $
    return ()
  curStep .= 0
  safe <- innerLoop
  if safe
    then break $ return True
    else continue $ do a <- use alpha
                       t *= a
                       return False

innerLoop :: M Bool
innerLoop = while False ((>) <$> use step <*> use curStep) $ do
  debug l $ curStep += 1
  a' <- sample
  fpst' <- (flip const) a' <$> use fpst
  traceM "UPDATE fpst !!!"
  (safe, sol') <- runSolve fpst'
  let cost'  = calculateCost (fpst'^.fpAnnotations) sol'
  let updAct = updateSol fpst' sol' cost'
  if safe
    then break $ updAct >> return True
    else continue $
         do p <- acceptanceProb cost'
            r <- liftIO randomIO
            when (p > r) updAct
            return False

updateSol :: FPSt -> Sol -> Double -> M ()
updateSol fpst' sol' cost' = do
  fpst     .= fpst'
  solution .= sol'
  cost     .= cost'

runSolve :: FPSt -> M (Bool, Sol)
runSolve f = do
  (liftIO1 $ flip solve f) =<< use cfg

sample :: M AnnotSt
sample = use (fpst.fpAnnotations)

acceptanceProb :: Double -> M Double
acceptanceProb newCost = do
  oldCost     <- use cost
  currentTemp <- use t
  return $ exp ( (oldCost - newCost) / currentTemp )

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

l :: String
l = cp 80 '-'

cp :: Int -> a -> [a]
cp n a = take n $ repeat a

debug :: MonadIO m => String -> m a -> m a
debug s act = (liftIO $ putStrLn s) >> act

debugM :: MonadIO m => m String -> m a -> m a
debugM ms act = ms >>= liftIO1 putStrLn >> act
