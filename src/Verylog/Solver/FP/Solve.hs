{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.FP.Solve ( solve
                               ) where

import Verylog.Language.Types
import Verylog.Solver.Common
import Verylog.Solver.FP.Types
import Verylog.Solver.FP.FQ
import Verylog.Utils

import qualified Language.Fixpoint.Solver       as F
import qualified Language.Fixpoint.Types        as FT
import qualified Language.Fixpoint.Types.Config as FC

import           Control.Lens hiding ((<.>))
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S
import           System.Console.ANSI
import           Text.PrettyPrint
import           Text.Printf

-- -----------------------------------------------------------------------------
solve :: FC.Config -> FPSt -> IO (Bool, FT.FixSolution)
-- -----------------------------------------------------------------------------
solve cfg fpst = do
  let finfo = toFqFormat fpst
  res <- silence $ F.solve cfg finfo
  let stat = FT.resStatus res
  colorStrLn (getColor stat) (render $ FT.resultDoc $ fmap fst stat)
  printResult fpst res
  return ( FT.isSafe res
         , FT.resSolution res
         )

-- -----------------------------------------------------------------------------
-- Printing results
-- -----------------------------------------------------------------------------

printResult :: FPSt -> FT.Result (Integer, HornId) -> IO ()
printResult fpst (FT.Result{..}) =
  case resStatus of
    FT.Unsafe ids -> do
      let m        = errMap ids
          findAB i = fromJust $ find (\a -> (a^.aId) == i) (fpst ^. fpABs)
      sequence_ $ (flip map) (M.assocs m) $ \(aid, cids) -> do
        withColor Blue $ printf "Failed constraint ids: %s\n" (show $ S.toList cids)
        print $ view aStmt $ findAB aid
    _          -> return ()
  where
    errMap cids = foldr (\(cid,hid) m ->
                           foldr (\(a_id, inv_type) m' ->
                                     M.alter (altr cid inv_type) a_id m'
                                 ) m (aIds hid)
                        ) M.empty cids

    aIds (HornId a2 t@(InvInter a1)) = [(a1, InvInter a2), (a2, t)]
    aIds (HornId a t)                = [(a,t)]

    altr cid t Nothing  = Just $ S.singleton (cid, t)
    altr cid t (Just s) = Just $ S.insert (cid, t) s

colorStrLn   :: Color -> String -> IO ()
colorStrLn c = withColor c . putStrLn

withColor :: Color -> IO () -> IO ()
withColor c act = do
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
  act
  setSGR [ Reset]

getColor :: FT.FixResult a -> Color
getColor (FT.Safe) = Green
getColor (_) = Red
