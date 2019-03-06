{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}

module Verylog.Runner ( VerylogArgs(..)
                      , verylogArgs
                      , run
                      , main
                      ) where

import qualified Verylog.Abduction as VA
import           Verylog.Pipeline
import           Verylog.Solver.FP.Solve
import           Verylog.Language.Parser
import           Verylog.Language.Types
import           Verylog.Solver.FP.FQ

import Language.Fixpoint.Types (saveQuery)
import Language.Fixpoint.Types.Config as FC

import Control.Exception
import System.Console.CmdArgs.Implicit
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Printf
import GHC.IO.Handle

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------
{- |
@
vcgen-fp v1.0, (C) Rami Gokhan Kici 2019

vcgen-fp [OPTIONS] FILE MODULE

Common flags:
     --iverilog-dir=DIR        path of the iverilog-parser directory
     --ir                      just generate the IR file
  -v --vcgen                   just generate the .fq file
  -m --minimize                run delta-debugging of fixpoint
     --no-save --nosave        do not save the fq file
  -a --abduction               run abduction algorithm
  -t --time                    print the runtime
     --no-output --nofpoutput  disable the output from fixpoint
  -h --help                    Display help message
  -V --version                 Print version information
     --numeric-version         Print just the version number
@

Checks whether the given Verilog file runs in constant time.
First argument is the path the to the verilog file.
Second argument is the name of the root Verilog module in that file.
-}
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

-- | Default arguments for the @VerylogArgs@ type.
-- @fileName@ and @moduleName@ are required.
-- By default, this project and iverilog-parser is assumed to be located in the same folder.
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
-- | Parses the command line arguments automatically, and runs the tool.
main = parseOpts >>= run

-- -----------------------------------------------------------------------------
run :: VerylogArgs -> IO ()
-- -----------------------------------------------------------------------------
-- | Runs the verification process.
run a = (normalizePaths a >>= generateIR >>= checkIR) `catch` peHandle `catch` passHandle

parseOpts :: IO VerylogArgs
parseOpts = cmdArgs verylogArgs

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
checkIR (VerylogArgs{..}) = makeSilent $ do
  fileContents <- readFile fileName

  fpst <- do
    res <- try $ evaluate $ pipeline (fileName, fileContents)
    case res of
      Left (PassError msg)  -> hPutStrLn stderr msg >> exitFailure
      Left (CycleError{..}) -> do
        let dotFileName = fileName <.> "dot"
        writeFile dotFileName cycleStr
        hPutStrLn stderr cycleErrorStr
        hPutStrLn stderr "\nCYCLE DETECTED !\n"
        exitFailure
      Right r -> return r

  if | vcgen     -> saveQuery cfg (toFqFormat fpst)
     | abduction -> VA.abduction cfg{save=False} fpst
     | otherwise -> solve cfg fpst >>= exitWith . fst

  where
    makeSilent = if noFPOutput then silence else id
    cfg = defConfig { eliminate   = Some
                    , save        = not noSave
                    , srcFile     = fileName
                    , metadata    = True
                    , FC.minimize = minimize
                    }


-- -----------------------------------------------------------------------------
-- Common Functions
-- -----------------------------------------------------------------------------

peHandle :: IRParseError -> IO ()
peHandle e = renderError e >>= hPutStrLn stderr >> exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg)  = hPutStrLn stderr msg >> exitFailure
passHandle (CycleError{..}) = hPutStrLn stderr cycleErrorStr >> exitFailure

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
