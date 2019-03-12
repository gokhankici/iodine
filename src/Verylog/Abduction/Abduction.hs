{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Abduction.Abduction (abduction) where

import Prelude hiding (break)

import Verylog.Solver.FP.Solve
import Verylog.Solver.FP.Types
import Verylog.Transform.FP.VCGen
import Verylog.Transform.Utils
import Verylog.Language.Types
import Verylog.Utils

import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Sequence       as SQ
import           Data.Hashable
import           Data.List (foldl', intercalate)
import           Data.Foldable (toList)
import           System.Random
import           Text.Printf

import Debug.Trace

type Sol = FT.FixSolution
type M   = StateT S IO

data GlobalMetadata =
  GlobalMetadata { _gmVariables :: SQ.Seq Id
                 }

-- state used in the annotation algorithm
data S = S { _t       :: Double -- temperature
           , _tMin    :: Double -- minimum temperature
           , _alpha   :: Double -- temperature update factor
           , _step    :: Int    -- # steps taken in each temperature
           , _curStep :: Int    -- current step (between 0 and step)

           , _solution :: Sol     -- solution returned by liquid-fixpoint
           , _isSafe   :: Bool
           , _cost     :: Double  -- the cost of the current annotations & solution
           , _fpst     :: FPSt    -- current solution

           , _cfg :: FC.Config

           , _globalMd :: GlobalMetadata

           -- , _negAnnots :: AnnotSt
           }

makeLenses ''GlobalMetadata
makeLenses ''S

--------------------------------------------------------------------------------
abduction :: FC.Config -> FPSt -> IO (Bool, Sol)
--------------------------------------------------------------------------------
abduction fcConfig st = do
  (safe, sol) <- solve fcConfig st
  let r = toR sol
  putStrLn $ show r

  let initialState =
        S { _t         = 1.0
          , _tMin      = 1e-5
          , _alpha     = 0.1
          , _step      = 2
          , _curStep   = 0
          , _isSafe    = safe
          , _solution  = sol
          , _cost      = calculateCost st (safe,sol)
          , _fpst      = st
          , _cfg       = fcConfig
          , _globalMd  = trace (show gmd) gmd
          -- , _negAnnots = mempty
          }

  (isSafe', s') <- runStateT outerLoop initialState
  let sol' = s' ^. solution
      ast' = s' ^. fpst ^. fpAnnotations

  traceM $ show ast'

  return (isSafe', sol')

  where
    gmd = collectMd (st^.fpABs)

    collectMd :: [AlwaysBlock] -> GlobalMetadata
    collectMd =
      let f m a = over gmVariables (upd a) m
          upd a = let ws = s2sq $ a ^. aMd ^. mRegisters
                      rs = s2sq $ a ^. aMd ^. mWires
                  in  (SQ.><) (rs SQ.>< ws)
      in foldl' f mempty

    s2sq :: HS.HashSet a -> SQ.Seq a
    s2sq = HS.foldl' (SQ.|>) SQ.empty

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
  fpst' <- sample
  (safe', sol') <- runSolve fpst'
  let cost'  = calculateCost fpst' (safe',sol')
  p <- acceptanceProb cost'
  traceM $ printf "acceptance prob = %g" p
  ifM (fmap (p >) randM)
    (updateSol safe' fpst' sol' cost')
    (traceM $ printf "skipping solution ::: %s" (show $ fpst'^.fpAnnotations)
    )
  ifM (use isSafe)
    (break $ return True)
    (continue $ return False)

sample :: M FPSt
sample = do
  ast' <- use (globalMd.gmVariables)
          >>=
          randomSample
          >>=
          randomVar
  toFpSt' ast' <$> use fpst

  where
    randomVar :: Id -> M AnnotSt
    randomVar v =
      chooseM
      (over sanitize     (HS.insert v) <$> use (fpst . fpAnnotations))
      (over sanitizeGlob (HS.insert v) <$> use (fpst . fpAnnotations))
      
calculateCost :: FPSt -> (Bool, Sol) -> Double
calculateCost st (safe, _) =
  if   safe
  then 0.0
  else costs
  where
    costs = initEqTotalCost + alwaysEqTotalCost + extraCost1

    initEqCost        = 1.0 :: Double
    alwaysEqCost      = 100.0 :: Double
    initEqTotalCost   = sz ies * initEqCost
    alwaysEqTotalCost = sz aes * alwaysEqCost

    sz :: HS.HashSet a -> Double
    sz = fromIntegral . HS.size

    extraCost1 :: Double
    extraCost1 =
      let f c v = if | v `HS.member` ies -> c + initEqCost   * 10.0
                     | v `HS.member` aes -> c + alwaysEqCost ^ (2::Int)
                     | otherwise         -> c
      in foldl' f 0.0 (ast^.sinks)

    ast = st ^. fpAnnotations
    ies = ast ^. sanitize
    aes = ast ^. sanitizeGlob

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

updateSol :: Bool -> FPSt -> Sol -> Double -> M ()
updateSol safe' fpst' sol' cost' = do
  traceM $ printf "UPDATING SOLUTION ::: %s" (show $ fpst'^.fpAnnotations)
  isSafe   .= safe'
  fpst     .= fpst'
  solution .= sol'
  cost     .= cost'

runSolve :: FPSt -> M (Bool, Sol)
runSolve f = do
  (liftIO1 $ flip solve f) =<< use cfg

acceptanceProb :: Double -> M Double
acceptanceProb newCost = do
  oldCost     <- use cost
  currentTemp <- use t
  return $ exp ( (oldCost - newCost) / currentTemp )

randM :: (Random a, MonadIO m) => m a
randM = liftIO randomIO

-- | Run one of the actions randomly and return the result
chooseM :: MonadIO m => m a -> m a -> m a
chooseM = ifM randM

randomSample :: MonadIO m => SQ.Seq a -> m a
randomSample sq =
  if   SQ.length sq == 0
  then error "randomSample is called with an empty sequence !"
  else (SQ.index sq) . (`mod` (SQ.length sq)) <$> randM

-- -----------------------------------------------------------------------------
-- Parsing liquid-fixpoint output
-- -----------------------------------------------------------------------------

data R = TagEq    { varName :: Id }
       | ValueEq  { varName :: Id }
       | TagEq2   { varName :: Id, var2Name :: Id }
       | ValueEq2 { varName :: Id, var2Name :: Id }
       | NoTaint  { varName :: Id }
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
        (f1, v1)   = parseVarName . str2Id $ FT.symbolSafeString s1
        (f2, v2)   = parseVarName . str2Id $ FT.symbolSafeString s2

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

instance Monoid GlobalMetadata where
  mempty = GlobalMetadata SQ.empty
  m1 `mappend` m2 =
    over gmVariables (SQ.>< m2 ^. gmVariables) $
    m1

instance Show GlobalMetadata where
  show gm = printf "GlobalMetadata(vars=[%s])"
            (intercalate ", " (fmap id2Str $ toList (gm^.gmVariables)))
