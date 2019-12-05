{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# OPTIONS_GHC -fno-cse #-}
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
iodine v2.0, (C) Rami Gokhan Kici 2019

iodine [OPTIONS] FILE FILE

Common flags:
     --iverilog-dir=DIR  path of the iverilog-parser directory
     --print-ir          just run the verilog parser
     --vcgen             just generate the .fq file
     --no-save           do not save the fq file
     --abduction         run abduction algorithm
     --no-fp-output      disable the output from fixpoint
     --verbose           enable verbose output
  -h --help              Display help message
  -V --version           Print version information
     --numeric-version   Print just the version number

Verifies whether the given Verilog file runs in constant time.

First argument is the path the to the verilog file.
Second argument is a JSON file that contains the annotations.
-}
data IodineArgs =
  IodineArgs { fileName    :: FilePath -- this is used for both the Verilog and IR file
             , annotFile   :: FilePath
             , iverilogDir :: FilePath
             , printIR     :: Bool
             , vcgen       :: Bool
             , noSave      :: Bool
             , noFPOutput  :: Bool
             , enableTrace :: Bool
             , abduction   :: Bool
             , verbose     :: Bool
             , moduleName  :: String
             }
  deriving (Show, Data, Typeable)

verylogArgs :: IodineArgs
verylogArgs = IodineArgs { fileName    = def
                                          &= argPos 0
                                          &= typFile
                          , annotFile   = def
                                          &= argPos 1
                                          &= typFile
                          , iverilogDir = "iverilog-parser"
                                          &= typDir
                                          &= explicit &= name "iverilog-dir"
                                          &= help "path of the iverilog-parser directory"
                          , printIR     = def
                                          &= explicit &= name "print-ir"
                                          &= help "just run the verilog parser"
                          , vcgen       = def
                                          &= explicit &= name "vcgen"
                                          &= help "just generate the .fq file"
                          , noSave      = def
                                          &= explicit &= name "no-save"
                                          &= help "do not save the fq file"
                          , noFPOutput  = def
                                          &= explicit &= name "no-fp-output"
                                          &= help "disable the output from fixpoint"
                          , enableTrace = def
                                          &= explicit &= name "trace"
                                          &= help "disable the debug trace"
                          , abduction   = def
                                          &= explicit &= name "abduction"
                                          &= help "run abduction algorithm"
                          , verbose     = def
                                          &= explicit &= name "verbose"
                                          &= help "enable verbose output"
                          , moduleName  = def
                                          &= explicit &= name "top-module"
                                          &= help "name of the top module"
                                          &= ignore
                          }
              &= program programName
              &= summary summaryText
              &= details detailsText
              &= helpArg [explicit, name "h", name "help"]
  where
    programName = "iodine"
    summaryText = printf "%s v2.0, (C) Rami Gokhan Kici 2019" programName :: String
    detailsText = [ "Verifies whether the given Verilog file runs in constant time."
                  , ""
                  , "First argument is the path the to the verilog file."
                  , "Second argument is a JSON file that contains the annotations."
                  ]


-- | Parses the command line arguments (e.g. from 'getArgs') into 'IodineArgs'.
parseArgs :: [String] -> IO IodineArgs
parseArgs as = withArgs as $ cmdArgs verylogArgs
