{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verylog.Runner ( Flag(..)
                      , Options(..)
                      , defaultOptions
                      , run
                      ) where

import Verylog.Abduction
import Verylog.Pipeline
import Verylog.Solver.FP.Types
import Verylog.Solver.Common
import Verylog.Language.Parser
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
import           System.FilePath.Posix
import           System.Environment (getArgs)
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

data Options = Options { optInputFile  :: FilePath
                       , optOutputFile :: FilePath
                       , optVCGen      :: Bool
                       , optPrintFInfo :: Bool
                       , optVisualize  :: Bool
                       , optMinimize   :: Bool
                       , optNoSave     :: Bool
                       , optAbduction  :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { optInputFile  = ""
                         , optOutputFile = ""
                         , optVCGen      = False
                         , optPrintFInfo = False
                         , optVisualize  = False
                         , optMinimize   = False
                         , optNoSave     = False
                         , optAbduction  = False
                         } 

run :: Options -> IO ()
run (Options{..}) = do
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
          let stat = resStatus res
          colorStrLn (getColor stat) (render $ resultDoc $ fmap fst stat)
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


-- -----------------------------------------------------------------------------
-- Common Functions
-- -----------------------------------------------------------------------------

peHandle :: IRParseError -> IO ()
peHandle e = renderError e >>= hPutStrLn stderr >> exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg) = do
  redError msg
  exitFailure
passHandle (CycleError{..}) = do
  hPutStrLn stderr cycleErrorStr
  exitFailure

redError :: String -> IO ()
redError msg = do
  hSetSGR stderr [ SetColor Foreground Vivid Red
                 , SetConsoleIntensity BoldIntensity
                 ]
  hPutStrLn stderr msg
  hSetSGR stderr []

getFiles :: IO (FilePath, FilePath)
getFiles = do
  args <- getArgs
  case args of
    [f1,f2] -> return (f1,f2)
    _       -> error "Please run with two files (input & output)"


type PRType = FilePath -> FilePath -> IO ()

runMain              :: PRType -> IO ()
runMain printResults = (getFiles >>= uncurry printResults) `catch` peHandle `catch` passHandle
