{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}

module Iodine.Runner ( IodineArgs(..)
                      , parseArgs
                      , run
                      , main
                      ) where

import qualified Iodine.Abduction.Runner as VAR
import           Iodine.Language.Parser
import           Iodine.Language.Types
import           Iodine.Pipeline
import           Iodine.Solver.FP.FQ
import           Iodine.Solver.FP.Solve
-- import           Iodine.Transform.FP.VCGen

import Language.Fixpoint.Types (saveQuery)
import Language.Fixpoint.Types.Config as FC

import Control.Exception
import Control.Monad
import System.Console.CmdArgs.Implicit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Printf

-- import Debug.Trace
-- import Control.DeepSeq
import qualified Data.ByteString.Lazy as B

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------
{- |
@
iodine v1.0, (C) Rami Gokhan Kici 2019

iodine [OPTIONS] FILE MODULE ANNOT_FILE

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

'fileName' and 'moduleName' are required:
First argument is the path the to the verilog file.
Second argument is the name of the root Verilog module in that file.
Third argument is a JSON file that contains the annotations.

By default, this project and @iverilog-parser@ is assumed to be located in the same folder.
-}
data IodineArgs =
  IodineArgs { fileName    :: FilePath -- this is used for both the Verilog and IR file
             , moduleName  :: String
             , iverilogDir :: FilePath
             , ir          :: Bool
             , vcgen       :: Bool
             , minimize    :: Bool
             , noSave      :: Bool
             , abduction   :: Bool
             , time        :: Bool
             , noFPOutput  :: Bool
             , annotFile   :: FilePath
             }
  deriving (Show, Data, Typeable)

verylogArgs :: IodineArgs
verylogArgs = IodineArgs { fileName    = def
                                          &= argPos 0
                                          &= typ "FILE"
                          , moduleName  = def
                                          &= argPos 1
                                          &= typ "MODULENAME"
                          , annotFile   = def
                                          &= argPos 2
                                          &= typ "ANNOT_FILE"
                          , iverilogDir = "iverilog-parser"
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
    programName = "iodine"
    summaryText = printf "%s v1.0, (C) Rami Gokhan Kici 2019" programName :: String
    detailsText = [ "Checks whether the given Verilog file runs in constant time."
                  , ""
                  , "First argument is the path the to the verilog file."
                  , "Second argument is the name of the root Verilog module in that file."
                  , "Third argument is a JSON file that contains the annotations."
                  ]


-- -----------------------------------------------------------------------------
main :: IO ()
-- -----------------------------------------------------------------------------
-- | Parses the command line arguments automatically, and runs the tool.
-- If the program is not constant time, the process exists with a non-zero return code.
main = do
  safe <- getArgs >>= parseArgs >>= run
  unless safe exitFailure

-- -----------------------------------------------------------------------------
run :: IodineArgs -> IO Bool
-- -----------------------------------------------------------------------------
-- | Runs the verification process, and returns 'True' if the program is constant time.
run a = (normalizePaths a >>= generateIR >>= checkIR) `catch` peHandle `catch` passHandle

-- | Parses the command line arguments (e.g. from 'getArgs') into 'IodineArgs'.
parseArgs :: [String] -> IO IodineArgs
parseArgs as = withArgs as $ cmdArgs verylogArgs

normalizePaths :: IodineArgs -> IO IodineArgs
normalizePaths IodineArgs{..} = do
  f' <- makeAbsolute fileName
  i' <- makeAbsolute iverilogDir
  a' <- makeAbsolute annotFile
  return IodineArgs { fileName    = f'
                    , iverilogDir = i'
                    , annotFile   = a'
                    , ..
                    }


-- -----------------------------------------------------------------------------
generateIR :: IodineArgs -> IO IodineArgs
-- -----------------------------------------------------------------------------
generateIR IodineArgs{..} = do
  runPreProcessor
  runIVL
  return result

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
      (rc, _out, err) <- readProcessWithExitCode ivl ivlArgs ""
      case rc of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          printMsg "Generating IR from the following Verilog file failed:" err
          exitFailure

    printMsg msg err = 
      forM_ (msg:[verilogFile, preprocFile, err]) (hPutStrLn stderr)

    verilogFile = fileName
    verilogDir  = takeDirectory verilogFile
    filePrefix  = verilogDir </> "" <.> dropExtensions (takeFileName verilogFile)
    preprocFile = filePrefix <.> "preproc" <.> "v"
    irFile      = filePrefix <.> "pl"
    result      = IodineArgs { fileName = irFile
                              , ..
                              }


-- -----------------------------------------------------------------------------
checkIR :: IodineArgs -> IO Bool
-- -----------------------------------------------------------------------------
checkIR IodineArgs{..} = do
  annotContents <- B.readFile annotFile
  fileContents <- readFile fileName
  let pipelineInput = ((fileName, fileContents), annotContents)
      fpst          = pipeline pipelineInput

  if | vcgen     -> saveQuery cfg (toFqFormat fpst) >> return True
     | abduction -> do let i = pipeline' pipelineInput
                       -- let input = VAR.runner' $ i
                       -- (safe, _) <- solve cfg $ toFpSt input
                       VAR.runner3 i
                       return True
     | otherwise -> fmap fst (solve cfg fpst)

  where
    cfg = defConfig { eliminate   = Some
                    , save        = not noSave
                    , srcFile     = fileName
                    , metadata    = True
                    , FC.minimize = minimize
                    }


-- -----------------------------------------------------------------------------
-- Common Functions
-- -----------------------------------------------------------------------------

peHandle :: IRParseError -> IO Bool
peHandle e = renderError e >>= hPutStrLn stderr >> return False

passHandle :: PassError -> IO Bool
passHandle (PassError msg)  = hPutStrLn stderr msg >> return False
passHandle CycleError{..} = do
  writeFile "/tmp/cycle.dot" cycleStr
  hPutStrLn stderr "Cycle is written to /tmp/cycle.dot"
  hPutStrLn stderr cycleErrorStr
  return False
