{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Abduction

import Verylog.MainCommon
import Verylog.FPGen
import Verylog.Solver.FP.Types
import Verylog.Solver.Common
import Verylog.Language.Types
import Verylog.Solver.FP.FQ

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import           Data.List
import           Data.Maybe
import           Control.Exception
import           Control.Lens hiding ((<.>))
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.FilePath.Posix
import           System.Exit
import           System.IO
import           Text.PrettyPrint
import           Text.Printf

data Flag = VCGen
          | PrintFInfo
          | Visualize
          | Minimize
          | NoSave
          | Abduction
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"]       (NoArg VCGen)      "Just vcgen, do not solve"
  , Option [] ["print-finfo"] (NoArg PrintFInfo) "Just vcgen, do not solve"
  , Option [] ["visualize"]   (NoArg Visualize)  "Visualize assignments"
  , Option [] ["minimize"]    (NoArg Minimize)   "print minimal failing constraint set"
  , Option [] ["no-save"]     (NoArg NoSave)     "do not save fq* files"
  , Option [] ["abduction"]   (NoArg Abduction)  "find hidden assumptions"
  ]

data Options = Options { optInputFile  :: FilePath
                       , optOutputFile :: FilePath
                       , optVCGen      :: Bool
                       , optPrintFInfo :: Bool
                       , optVisualize  :: Bool
                       , optMinimize   :: Bool
                       , optNoSave     :: Bool
                       , optAbduction  :: Bool
                       }

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let [fin, fout] = rest
        in Options { optInputFile  = fin
                   , optOutputFile = fout
                   , optVCGen      = VCGen      `elem` opts
                   , optPrintFInfo = PrintFInfo `elem` opts
                   , optVisualize  = Visualize  `elem` opts
                   , optMinimize   = Minimize   `elem` opts
                   , optNoSave     = NoSave     `elem` opts
                   , optAbduction  = Abduction  `elem` opts
                   }
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."
  
main :: IO ()
main  = do
  Options{..} <- parseOpts

  fileContents <- readFile optInputFile

  fpst <- do
    res <- try $ evaluate $ pipeline optInputFile fileContents
    case res of
      Left (PassError msg)  -> hPutStrLn stderr msg >> exitFailure
      Left (CycleError{..}) -> do
        let dotFileName = optInputFile <.> "dot"
        writeFile dotFileName cycleStr
        hPutStrLn stderr cycleErrorStr
        redError "\nCYCLE DETECTED !\n"
        exitFailure
      Right r -> return r

  let finfo = toFqFormat fpst

  let cfg = defConfig{ eliminate = Some
                     , save      = not optNoSave
                     , srcFile   = optInputFile
                     , metadata  = True
                     , minimize  = optMinimize
                     -- , cores     = Just 4
                     } 

  case () of
    _ | optVCGen      ->
        if   optNoSave
        then putStrLn $ intercalate "\n\n" (show <$> fpst ^. fpABs)
        else saveQuery cfg finfo >> exitSuccess
      | optPrintFInfo -> do
          fInfo <- parseFInfo [optOutputFile] :: IO (FInfo ())
          putStrLn $ show fInfo
          exitSuccess
      | optAbduction  -> abduction cfg{save=False} fpst
      | otherwise     -> do
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
withColor c act
   = do setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
        act
        setSGR [ Reset]

getColor        :: FixResult a -> Color
getColor (Safe) = Green
getColor (_)    = Red
