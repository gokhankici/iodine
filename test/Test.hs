{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Verylog.Runner as R

import Control.Exception
import Control.Monad
import System.FilePath.Posix
import System.Exit
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec

data UnitTestType = Succ | Fail
                  deriving (Eq, Show)

data UnitTest = UnitTest { testName    :: String
                         , moduleName  :: String
                         , verilogFile :: FilePath
                         , testType    :: UnitTestType
                         }

-- -----------------------------------------------------------------------------
-- SIMPLE TESTS
-- -----------------------------------------------------------------------------
simple :: Runner -> FilePath -> Spec
simple runner testDir = do
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
            , "merge04"
            , "secverilog-01"
            ]


-- -----------------------------------------------------------------------------
-- MIPS TESTS
-- -----------------------------------------------------------------------------
mips :: Runner -> FilePath -> Spec
mips runner d = do
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
            , "neg-merge-01"
            , "neg-test-11"
            , "secverilog-neg-01"
            , "secverilog-neg-02"
            ]


-- -----------------------------------------------------------------------------
-- MAJOR TESTS
-- -----------------------------------------------------------------------------
major :: Runner -> FilePath -> Spec
major runner parserDir = describe "major" $ forM_ ts runner
  where
    b  = benchmarkDir parserDir
    c  = b </> "crypto_cores"
    ts = [ t { testName    = "mips"
             , moduleName  = "mips_pipeline"
             , verilogFile = mipsDir parserDir </> "mips_pipeline.v"
             }
         , t { testName    = "yarvi"
             , moduleName  = "yarvi"
             , verilogFile = b </> "yarvi/shared/yarvi.v"
             }
         , t { testName    = "sha256"
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

spec :: Spec
spec = sequential $ do
  simple r testDir
  negative r testDir
  mips r $ mipsDir parserDir
  major r parserDir
  where
    thisDir   = "."
    parserDir = R.iverilogDir R.verylogArgs
    r         = runUnitTest (thisDir </> "verylog")
    testDir   = thisDir </> "test"

main :: IO ()
main = hspecWith cfg spec
  where
  cfg = defaultConfig { configFastFail      = True
                      , configFailureReport = Just reportPath
                      , configPrintCpuTime  = True
                      }

-- default unit test
t :: UnitTest
t = UnitTest { testName    = undefined
             , moduleName  = undefined
             , verilogFile = undefined
             , testType    = Succ
             }

type Runner = UnitTest -> Spec

-- given the parser dir, returns the benchmark's root directory
benchmarkDir, mipsDir :: FilePath -> FilePath
benchmarkDir p = p </> "benchmarks"
mipsDir p      = benchmarkDir p </> "472-mips-pipelined"

reportPath :: String
reportPath = "test-report.txt"

runUnitTest :: FilePath -> Runner
runUnitTest scriptPath (UnitTest{..}) =
  it testName $ act `shouldReturn` testType
  where
    a = R.verylogArgs { R.fileName   = verilogFile
                      , R.moduleName = moduleName
                      , R.noSave     = True
                      , R.noFPOutput = True
                      }
    act = (R.run a >> return Succ) `catch` handler
    handler ExitSuccess = return Succ
    handler _           = return Fail
      
