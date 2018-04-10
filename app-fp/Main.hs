{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.FPGen
import Verylog.Solver.FP.Types
import Verylog.Solver.Common
import Verylog.Language.Types

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import           Data.List
import           Data.Maybe
import           Control.Lens
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit
import           Text.PrettyPrint
import           Text.Printf

data Flag = VCGen
          | PrintFInfo
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"] (NoArg VCGen) "Just vcgen, do not solve"
  , Option [] ["print-finfo"] (NoArg PrintFInfo) "Just vcgen, do not solve"
  ]

parseOpts :: IO (FilePath, FilePath, Bool, Bool)
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let [fin, fout] = rest
            skip        = VCGen `elem` opts
            prfinfo     = PrintFInfo`elem` opts
        in  (fin, fout, skip, prfinfo)
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."
  

main :: IO ()
main  = do
  (fin, fout, skipSolve, prFInfo) <- parseOpts

  (fpst, finfo) <- fpgen fin
  let cfg = defConfig{ eliminate = Some
                     , save      = True
                     , srcFile   = fin
                     , metadata  = True
                     } 

  case () of
    _ | skipSolve -> saveQuery cfg finfo >> exitSuccess
      | prFInfo   -> do
          fInfo <- parseFInfo [fout] :: IO (FInfo ())
          putStrLn $ show fInfo
          exitSuccess
      | otherwise -> do
          res <- solve cfg finfo
          let statStr = render . resultDoc . fmap fst
          let stat = resStatus res
          colorStrLn (getColor stat) (statStr stat)
          printResult fpst res
          exitWith (resultExit stat)

printResult :: FPSt -> Result (Integer, HornId) -> IO ()
printResult fpst (Result{..}) =
  case resStatus of
    Unsafe ids -> do
      let m        = errMap ids
          findAB i = fromJust $ find (\a -> (a^.aId) == i) (fpst ^. fpABs)
      sequence_ $ (flip map) (M.assocs m) $ \(aid, cids) -> do
        printf "Failed constraint ids: %s\n" (show $ S.toList cids)
        print $ findAB aid
    _          -> return ()
  where
    errMap ids = foldr (\(i,hid) m -> foldr (\a m' -> M.alter (altr i) a m') m (aIds hid)) M.empty ids

    aIds (SingleBlock a)           = [a]
    aIds (InterferenceBlock a2 a1) = [a2,a1]

    altr i Nothing  = Just $ S.singleton i
    altr i (Just s) = Just $ S.insert i s
                     

colorStrLn   :: Color -> String -> IO ()
colorStrLn c = withColor c . putStrLn

withColor :: Color -> IO () -> IO ()
withColor c act
   = do setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
        act
        setSGR [ Reset]

getColor        :: FixResult a -> Color
getColor (Safe) = Green
getColor (_)    = Red
