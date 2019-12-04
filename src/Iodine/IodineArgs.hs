{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Iodine.IodineArgs (IodineArgs(..), parseArgs) where

import System.Console.CmdArgs.Implicit
import System.Environment
import Text.Printf

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------
{- |
@
iodine v1.0, (C) Rami Gokhan Kici 2019

iodine [OPTIONS] FILE MODULENAME ANNOT_FILE

Common flags:
     --iverilog-dir=DIR        path of the iverilog-parser directory
     --ir                      just generate the IR file
     --vcgen                   just generate the .fq file
  -m --minimize                run delta-debugging of fixpoint
     --no-save --nosave        do not save the fq file
  -a --abduction               run abduction algorithm
  -t --time                    print the runtime
     --no-output --nofpoutput  disable the output from fixpoint
     --verbose                 enable verbose output
  -h --help                    Display help message
  -V --version                 Print version information
     --numeric-version         Print just the version number
@

Verifies whether the given Verilog file runs in constant time.

First argument is the path the to the verilog file.
Second argument is the name of the root Verilog module in that file.
Third argument is a JSON file that contains the annotations.
-}
data IodineArgs =
  IodineArgs { fileName    :: FilePath -- this is used for both the Verilog and IR file
             , moduleName  :: String
             , iverilogDir :: FilePath
             , printIR     :: Bool
             , ir          :: Bool
             , vcgen       :: Bool
             , minimize    :: Bool
             , noSave      :: Bool
             , abduction   :: Bool
             , time        :: Bool
             , noFPOutput  :: Bool
             , annotFile   :: FilePath
             , verbose     :: Bool
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
                          , printIR     = def
                                          &= explicit &= name "print-ir"
                                          &= help "just run the verilog parser"
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
                          , verbose     = def
                                          &= explicit &= name "verbose"
                                          &= help "enable verbose output"
                          }
              &= program programName
              &= summary summaryText
              &= details detailsText
              &= helpArg [explicit, name "h", name "help"]
  where
    programName = "iodine"
    summaryText = printf "%s v1.0, (C) Rami Gokhan Kici 2019" programName :: String
    detailsText = [ "Verifies whether the given Verilog file runs in constant time."
                  , ""
                  , "First argument is the path the to the verilog file."
                  , "Second argument is the name of the root Verilog module in that file."
                  , "Third argument is a JSON file that contains the annotations."
                  ]


-- | Parses the command line arguments (e.g. from 'getArgs') into 'IodineArgs'.
parseArgs :: [String] -> IO IodineArgs
parseArgs as = withArgs as $ cmdArgs verylogArgs
