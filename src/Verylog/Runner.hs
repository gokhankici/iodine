{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Verylog.Runner ( VerylogArgs(..)
                      , verylogArgs
                      , run
                      , main
                      ) where

import qualified Verylog.Abduction as VA
import Verylog.Pipeline
import Verylog.Solver.FP.Types
import Verylog.Solver.Common
import Verylog.Language.Parser
import Verylog.Language.Types
import Verylog.Solver.FP.FQ

import Language.Fixpoint.Solver
import Language.Fixpoint.Types hiding (err)
import Language.Fixpoint.Types.Config as FPConfig

import           Control.Exception
import           Control.Lens hiding ((<.>))
import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           System.Console.ANSI
import           System.Console.CmdArgs.Implicit
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process
import           Text.PrettyPrint
import           Text.Printf
import           GHC.IO.Handle

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------

data VerylogArgs =
  VerylogArgs { fileName    :: FilePath -- this is used for both the Verilog and IR file
              , moduleName  :: String
              , iverilogDir :: FilePath
              , ir          :: Bool
              , vcgen       :: Bool
              , minimize    :: Bool
              , noSave      :: Bool
              , abduction   :: Bool
              , time        :: Bool
              , noFPOutput  :: Bool
              }
  deriving (Show, Data, Typeable)

verylogArgs :: VerylogArgs
verylogArgs = VerylogArgs { fileName    = def
                                          &= argPos 0
                                          &= typ "FILE"
                          , moduleName  = def
                                          &= argPos 1
                                          &= typ "MODULE"
                          , iverilogDir = "../iverilog-parser"
                                          &= typDir
                                          &= explicit &= name "iverilog-dir"
                                          &= help "path of the iverilog-parser directory"
                          , ir          = def
                                          &= explicit &= name "ir"
                                          &= help "just generate the IR file"
                          , vcgen       = def
                                          &= explicit &= name "vcgen"
                                          &= help "just generate the .fq file"
                          , minimize    = def
                                          &= explicit &= name "minimize"
                                          &= help "run delta-debugging of fixpoint"
                          , noSave      = def
                                          &= explicit &= name "no-save"
                                          &= help "do not save the fq file"
                          , abduction   = def
                                          &= explicit &= name "abduction"
                                          &= help "run abduction algorithm"
                          , time        = def
                                          &= explicit &= name "time"
                                          &= help "print the runtime"
                          , noFPOutput  = def
                                          &= explicit &= name "no-output"
                                          &= help "disable the output from fixpoint"
                          }
              &= program programName
              &= summary summaryText
              &= details detailsText
              &= helpArg [explicit, name "h", name "help"]
  where
    programName = "vcgen-fp"
    summaryText = printf "%s v1.0, (C) Rami Gokhan Kici 2019" programName :: String
    detailsText = [ "Checks whether the given Verilog file runs in constant time."
                  , ""
                  , "First argument is the path the to the verilog file."
                  , "Second argument is the name of the root Verilog module in that file."
                  ]


-- -----------------------------------------------------------------------------
main :: IO ()
-- -----------------------------------------------------------------------------
main = parseOpts >>= run

parseOpts :: IO VerylogArgs
parseOpts = cmdArgs verylogArgs

run :: VerylogArgs -> IO ()
run a = (normalizePaths a >>= generateIR >>= checkIR) `catch` peHandle `catch` passHandle

normalizePaths :: VerylogArgs -> IO VerylogArgs
normalizePaths (VerylogArgs{..}) = do
  f' <- makeAbsolute fileName
  i' <- makeAbsolute iverilogDir
  return $ VerylogArgs { fileName    = f'
                       , iverilogDir = i'
                       , ..
                       }

-- -----------------------------------------------------------------------------
generateIR :: VerylogArgs -> IO VerylogArgs
-- -----------------------------------------------------------------------------
generateIR (VerylogArgs{..}) = runPreProcessor >> runIVL >> appendAnnots >> return result
  where
    -- run ivlpp preprocessor on the given verilog file
    runPreProcessor = withCurrentDirectory verilogDir $ do
      let preprocessor = iverilogDir </> "ivlpp" </> "ivlpp"
      (rc, out, err) <- readProcessWithExitCode preprocessor [verilogFile] ""
      case rc of
        ExitSuccess ->
          writeFile preprocFile out
        ExitFailure _ -> do
          hPutStrLn stderr "Preprocessing of the following file failed:"
          hPutStrLn stderr verilogFile
          hPutStrLn stderr err
          exitFailure

    -- compile the Verilog file into IR
    runIVL = do
      let ivl     = iverilogDir </> "ivl"
          ivlArgs = [ "-M", moduleName
                    , "-O", irFile
                    , preprocFile
                    ]
      (rc, out, err) <- readProcessWithExitCode ivl ivlArgs ""
      case rc of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          hPutStrLn stderr "Generating IR from the following Verilog file failed:"
          hPutStrLn stderr verilogFile
          hPutStrLn stderr preprocFile
          hPutStrLn stderr err
          exitFailure

    -- extract the annotations from the Verilog file and append them to the IR
    appendAnnots = do
      let rgx = "s|[^@]*@annot{\\([^}]*\\)}[^@]*|\\1.\\n|pg"
      (rc, out, err) <- readProcessWithExitCode "/bin/sed" ["-n", rgx, verilogFile] ""
      case rc of
        ExitSuccess -> do
          appendFile irFile out
        ExitFailure _ -> do
          hPutStrLn stderr "Parsing annotations failed:"
          hPutStrLn stderr verilogFile
          hPutStrLn stderr preprocFile
          hPutStrLn stderr err
          exitFailure

    verilogFile = fileName
    verilogDir  = takeDirectory verilogFile
    filePrefix  = verilogDir </> "" <.> (dropExtensions $ takeFileName verilogFile)
    preprocFile = filePrefix <.> "preproc" <.> "v"
    irFile      = filePrefix <.> "pl"
    result      = VerylogArgs { fileName = irFile
                              , ..
                              }

-- -----------------------------------------------------------------------------
checkIR :: VerylogArgs -> IO ()
-- -----------------------------------------------------------------------------
checkIR (VerylogArgs{..}) = do
  fileContents <- readFile fileName

  fpst <- do
    res <- try $ evaluate $ pipeline (fileName, fileContents)
    case res of
      Left (PassError msg)  -> hPutStrLn stderr msg >> exitFailure
      Left (CycleError{..}) -> do
        let dotFileName = fileName <.> "dot"
        writeFile dotFileName cycleStr
        hPutStrLn stderr cycleErrorStr
        redError "\nCYCLE DETECTED !\n"
        exitFailure
      Right r -> return r

  let finfo = toFqFormat fpst

  let cfg = defConfig { eliminate = Some
                      , save      = not noSave
                      , srcFile   = fileName
                      , metadata  = True
                      , FPConfig.minimize  = minimize
                      }

  case () of
    _ | vcgen ->
        if   noSave
        then putStrLn $ intercalate "\n\n" (show <$> fpst ^. fpABs)
        else saveQuery cfg finfo >> exitSuccess
      | abduction -> VA.abduction cfg{save=False} fpst
      | otherwise ->
        let act = do res <- solve cfg finfo
                     let stat = resStatus res
                     colorStrLn (getColor stat) (render $ resultDoc $ fmap fst stat)
                     printResult fpst res
                     exitWith (resultExit stat)
        in if   noFPOutput
           then silence act
           else act


-- -----------------------------------------------------------------------------
-- Printing results
-- -----------------------------------------------------------------------------

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

redError :: String -> IO ()
redError msg = do
  hSetSGR stderr [ SetColor Foreground Vivid Red
                 , SetConsoleIntensity BoldIntensity
                 ]
  hPutStrLn stderr msg
  hSetSGR stderr []


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

-- taken from https://github.com/hspec/silently
silence :: IO a -> IO a
silence action = bracket (openFile "/dev/null" AppendMode) hClose prepareAndRun
  where
    handles = [stdout, stderr]
    prepareAndRun tmpHandle = go handles
      where
        go [] = action
        go hs = goBracket go tmpHandle hs

    goBracket _ _ [] = undefined
    goBracket go tmpHandle (h:hs) = do
      buffering <- hGetBuffering h
      let redirect = do
            old <- hDuplicate h
            hDuplicateTo tmpHandle h
            return old
          restore old = do
            hDuplicateTo old h
            hSetBuffering h buffering
            hClose old
      bracket redirect restore (\_ -> go hs)
