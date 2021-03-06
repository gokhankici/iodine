{-# LANGUAGE RecordWildCards #-}

module Iodine.Solver.FP.Solve ( solve
                               ) where

import Iodine.Language.Types
import Iodine.Solver.Common
import Iodine.Solver.FP.Types
import Iodine.Solver.FP.FQ

import qualified Language.Fixpoint.Solver       as F
import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens
import           Data.List
import qualified Data.Map.Strict      as M
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import qualified Data.Set             as S
import           System.Console.ANSI
import           Text.PrettyPrint
import           Text.Printf

-- -----------------------------------------------------------------------------
solve :: FC.Config -> FPSt -> IO (Bool, FT.FixSolution)
-- -----------------------------------------------------------------------------
solve cfg fpst = do
  let finfo     = toFqFormat fpst
  res <- F.solve cfg finfo
  let stat = FT.resStatus res
  withColor (getColor stat) $ putStr (render $ FT.resultDoc $ fmap fst stat)
  putStrLn ""
  printUnsafeResult fpst res
  return ( FT.isSafe res
         , FT.resSolution res
         )
  where
    getColor FT.Safe = Green
    getColor _       = Red

-- -----------------------------------------------------------------------------
-- Printing results
-- -----------------------------------------------------------------------------

printUnsafeResult :: FPSt -> FT.Result (Integer, HornId) -> IO ()
printUnsafeResult fpst FT.Result{..} = do
  -- printf "%d always blocks in total" (length $ fpst ^. fpABs)
  case resStatus of
    FT.Unsafe ids -> do
      let m        = errMap ids
          findAB i = fromJust $ find (\a -> (a^.aId) == i) (fpst ^. fpABs)
      sequence_ $ flip map (M.assocs m) $ \(aid, cids) -> do
        withColor Blue $ printf "Failed constraint ids: %s\n" (show $ S.toList cids)
        print $ view aStmt $ findAB aid
      printSolution resSolution
    _          -> return ()
  where
    errMap = foldr (\(cid,hid) m ->
                      foldr (\(a_id, inv_type) m' ->
                                M.alter (altr cid inv_type) a_id m'
                            ) m (aIds hid)
                   ) M.empty

    aIds (HornId a2 t@(InvInter a1)) = [(a1, InvInter a2), (a2, t)]
    aIds (HornId a t)                = [(a,t)]

    altr cid t Nothing  = Just $ S.singleton (cid, t)
    altr cid t (Just s) = Just $ S.insert (cid, t) s

printSolution :: FT.FixSolution -> IO ()
printSolution sol = do
  withColor Blue $ putStrLn "\nFixpoint output:"
  mapM_ (putStrLn . FT.showpp) $ nub $ HM.elems sol >>= getExprs
  where
    getExprs (FT.PAnd es) = es >>= getExprs
    getExprs e            = [e]

withColor :: Color -> IO () -> IO ()
withColor c act = do
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
  act
  --setSGR [ Reset]
  setSGR []
