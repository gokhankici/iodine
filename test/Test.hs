{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import           Control.Exception
import           Control.Lens                    hiding (simple, (<.>))
import           Control.Monad
import           GHC.Generics                    hiding (moduleName, to)
import           GHC.IO.Handle
import qualified Iodine.IodineArgs               as IA
import qualified Iodine.Runner                   as R
import           System.Console.CmdArgs.Explicit
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Spec
import           Text.Printf

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------
data TestArgs = TestArgs { _verbose      :: Bool
                         , _help         :: Bool
                         , _iodineArgs   :: [String]
                         , _hspecArgs    :: [String] -- rest of the positional arguments
                         , _runAbduction :: Bool
                         , _dryRun       :: Bool
                         }
              deriving (Generic, Show)

makeLenses ''TestArgs

testArgs :: Mode TestArgs
testArgs = mode programName def detailsText (flagArg argUpd "HSPEC_ARG") flags
  where
    flags = [ flagReq ["iodine"] (\s -> Right . over iodineArgs (++ words s)) "IODINE_ARG"
              "This is passed to the Iodine script directly."
            , flagNone ["a", "abduction"] (set runAbduction True)
              "Only run the abduction tests, otherwise they are disabled."
            , flagNone ["v", "verbose"] (set verbose True)
              "Display both stdout & stderr of a test."
            , flagNone ["d", "dry-run"] (set dryRun True)
              "Print the calls to Iodine"
            , flagNone ["h", "help"] (set help True)
              "Displays this help message."
            ]

    argUpd s = Right . over hspecArgs (++ [s])

    programName = "iodine-test"
    detailsText = unlines [ "Runs the benchmarks."
                          , "The positional arguments (e.g. after --) are passed into hspec."
                          ]

    def = TestArgs { _verbose      = False
                   , _help         = False
                   , _iodineArgs   = []
                   , _hspecArgs    = []
                   , _runAbduction = False
                   , _dryRun       = False
                   }

parseOpts :: IO TestArgs
parseOpts = do
  res <-  fmap post . process testArgs <$> getArgs
  case res of
    Left errMsg -> error errMsg
    Right opts  -> do
      when (opts^.help) $ do
        print $ helpText [] HelpFormatDefault testArgs
        exitSuccess
      return opts
  where
    post o =
      if o ^. runAbduction
      then over hspecArgs  (++ ["--match", '/':abductionRoot]) .
           over iodineArgs (++ ["--abduction"]) .
           set  verbose    True $
           o
      else over hspecArgs (++ ["--skip",  '/':abductionRoot]) o


-- -----------------------------------------------------------------------------
-- Data Types
-- -----------------------------------------------------------------------------

data UnitTestType = Succ | Fail
                  deriving (Eq, Show)

data UnitTest = UnitTest { testName    :: String
                         , moduleName  :: String
                         -- verilog file contains the top level module
                         , verilogFile :: FilePath
                         -- JSON file that contains the annotations
                         -- Default value is "dir/annot-name.json" where
                         -- "dir/name.v" is the verilog file
                         , annotFile   :: Maybe FilePath
                         , testType    :: UnitTestType
                         }

-- -----------------------------------------------------------------------------
-- SIMPLE TESTS
-- -----------------------------------------------------------------------------
simple :: Runner -> FilePath -> Spec
simple runner testDir =
  describe "simple" $ do
    forM_ (go <$> names) runner
    runner $ t { testName    = "stall-hand"
               , moduleName  = "stalling_cpu"
               , verilogFile = takeDirectory testDir </> "examples/verilog/stall.v"
               }
  where
    posDir = testDir </> "verilog" </> "pos"
    go name = t { testName    = name
                , moduleName  = "test"
                , verilogFile = posDir </> name <.> "v"
                }
    names = [ "tr-test-1"
            , "tr-test-2"
            , "tr-test-3"
            , "tr-test-4"
            , "tr-test-5"
            , "tr-test-6"
            , "tr-test-9"
            , "tr-test-10"
            , "tr-test-11"
            , "merge-02"
            , "merge03"
            , "merge04-1"
            , "merge04"
            , "merge05"
            , "secverilog-01"
            , "nb-test-01"
            ]


-- -----------------------------------------------------------------------------
-- SIMPLE TESTS
-- -----------------------------------------------------------------------------
abductionRoot :: String
abductionRoot = "abduction"

abduction :: Runner -> FilePath -> Spec
abduction runner testDir = describe abductionRoot $ forM_ (go <$> names) runner
  where
    go name = t { testName    = name
                , moduleName  = "test"
                , verilogFile = testDir </> name <.> "v"
                }
    names = [ "abduction-01"
            ]

-- -----------------------------------------------------------------------------
-- MIPS TESTS
-- -----------------------------------------------------------------------------
mips :: Runner -> FilePath -> Spec
mips runner d =
  describe "mips" $ do
    mipsModules runner d
    mipsStubs   runner d

mipsModules :: Runner -> FilePath -> Spec
mipsModules runner d = describe "modules" $ forM_ (go <$> names) runner
  where
    go name = t { testName    = name
                , moduleName  = name
                , verilogFile = d </> name <.> "v"
                }
    names = [ "reg32"
            , "mux3"
            , "control_pipeline"
            , "alu"
            , "alu_ctl"
            , "rom32"
            , "reg_file"
            ]

mipsStubs :: Runner -> FilePath -> Spec
mipsStubs runner d = describe "stub" $ forM_ (go <$> names) runner
  where
    go (ver, name) = t { testName    = ver
                       , moduleName  = "mips_pipeline"
                       , verilogFile = d </> name <.> "v"
                       }
    names = [ ("v1", "472-mips-fragment")
            , ("v2", "472-mips-fragment-2")
            , ("v3", "472-mips-fragment-3")
            , ("v4", "472-mips-fragment-4")
            ]


-- -----------------------------------------------------------------------------
-- NEGATIVE TESTS
-- -----------------------------------------------------------------------------
negative :: Runner -> FilePath -> Spec
negative runner testPath = describe "negative" $ forM_ (go <$> names) runner
  where
    negDir = testPath </> "verilog/neg"
    go name = t { testName    = name
                , moduleName  = "test"
                , verilogFile = negDir </> name <.> "v"
                , testType    = Fail
                }
    names = [ "neg-test-1"
            , "neg-test-2"
            , "neg-test-5"
            , "tp"
            , "neg-test-11"
            , "secverilog-neg-01"
            , "secverilog-neg-02"
            ]


-- -----------------------------------------------------------------------------
-- MAJOR TESTS
-- -----------------------------------------------------------------------------
majorStubs :: Runner -> Spec
majorStubs runner = describe "major-stub" $ forM_ ts runner
  where
    b  = benchmarkDir
    d  = b </> "crypto_cores/sha_core/trunk/rtl"
    ts = [ UnitTest { testName    = "sha_stub"
                    , moduleName  = "sha256"
                    , verilogFile = d </> "sha256_stub.v"
                    , annotFile   = Just $ d </> "annot-sha256.json"
                    , testType    = Succ
                    }
         ]

major :: Runner -> FilePath -> Spec
major runner _parserDir = describe "major" $ forM_ ts runner
  where
    b  = benchmarkDir
    c  = b </> "crypto_cores"
    ts = [ t { testName    = "mips"
             , moduleName  = "mips_pipeline"
             , verilogFile = mipsDir </> "mips_pipeline.v"
             }
         , t { testName    = "yarvi"
             , moduleName  = "yarvi"
             , verilogFile = b </> "yarvi/shared/yarvi.v"
             }
         , t { testName    = "sha"
             , moduleName  = "sha256"
             , verilogFile = c </> "sha_core/trunk/rtl/sha256.v"
             }
         , t { testName    = "fpu"
             , moduleName  = "fpu"
             , verilogFile = b </> "fpu/verilog/fpu.v"
             }
         , t { testName    = "fpu-divider"
             , moduleName  = "divider"
             , verilogFile = b </> "fpu2/divider/divider.v"
             , testType    = Fail
             }
         , t { testName    = "modexp"
             , moduleName  = "ModExp"
             , verilogFile = c </> "RSA4096/ModExp2/ModExp.v"
             , testType    = Fail
             }
         , t { testName    = "ctalu"
             , moduleName  = "scarv_cop_palu"
             , verilogFile = b </> "xcrypto-ref/rtl/coprocessor/scarv_cop_palu.v"
             }
         ]


-- -----------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- parseOpts

  -- if no Iodine argument is given, use the following default ones
  let updateDef va =
        if null $  opts ^. iodineArgs
        then va { IA.noSave     = True
                , IA.noFPOutput = view (verbose . to not) opts
                }
        else va

  -- hack: set the required first two positional arguments to empty list
  va <- updateDef . invalidate <$> IA.parseArgs ("" : "" : "" : opts ^. iodineArgs)

  readConfig defaultConfig (opts^.hspecArgs)
    >>= withArgs [] . runSpec (spec opts va)
    >>= evaluateSummary
  where
    invalidate va = va { IA.fileName   = undefined
                       , IA.moduleName = undefined
                       , IA.annotFile  = undefined
                       }

type Runner = UnitTest -> Spec

spec :: TestArgs -> IA.IodineArgs -> Spec
spec ta va = sequential $ do
  simple r testDir
  negative r testDir
  mips r mipsDir
  abduction r $ testDir </> "abduction" </> "pos"
  majorStubs r
  major r parserDir
  where
    testDir   = "test"
    r         = runUnitTest ta va
    parserDir = IA.iverilogDir va

-- default unit test
t :: UnitTest
t = UnitTest { testName    = undefined
             , moduleName  = undefined
             , verilogFile = undefined
             , annotFile   = Nothing
             , testType    = Succ
             }

-- given the parser dir, returns the benchmark's root directory
benchmarkDir :: FilePath
benchmarkDir = "benchmarks"

mipsDir :: FilePath
mipsDir = benchmarkDir </> "472-mips-pipelined"

runUnitTest :: TestArgs -> IA.IodineArgs -> Runner
runUnitTest ta va UnitTest{..} =
  if   ta ^. dryRun
  then it testName (printf "iodine %s %s %s\n" verilogFile moduleName af :: IO ())
  else it testName $ (withSilence $ R.run va') `shouldReturn` (testType == Succ)
  where
    withSilence = if ta ^. verbose then id else silence
    af  = case annotFile of
            Nothing -> let dir  = takeDirectory verilogFile
                           name = dropExtension $ takeBaseName verilogFile
                       in  dir </> "annot-" ++ name <.> "json"
            Just f  -> f
    va' = va { IA.fileName   = verilogFile
             , IA.moduleName = moduleName
             , IA.annotFile  = af
             , IA.noSave     = True
             , IA.verbose    = ta ^. verbose
             }

silence :: IO a -> IO a
silence action = withFile "/dev/null" AppendMode prepareAndRun
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
