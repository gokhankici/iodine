{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}

module TestData where

import System.FilePath

data UnitTestType = Succ | Fail deriving (Eq, Show)

data UnitTest =
  UnitTest
  { testName    :: String
  , moduleName  :: String
  , verilogFile :: FilePath       -- | verilog file contains the top level module
  , annotFile   :: Maybe FilePath -- | JSON file that contains the annotations.
                                  -- Default value is "dir/annot-name.json".
                                  -- where "dir/name.v" is the verilog file
  , testType    :: UnitTestType
  }

-- | default unit test patterns
pattern T :: String -> String -> FilePath -> UnitTest
pattern T testName moduleName verilogFile =
  UnitTest { annotFile = Nothing
           , testType  = Succ
           , ..
           }

pattern TF :: String -> String -> FilePath -> UnitTest
pattern TF testName moduleName verilogFile =
  UnitTest { annotFile = Nothing
           , testType  = Fail
           , ..
           }

data TestTree = SingleTest     UnitTest
              | TestCollection String [TestTree]

mkCollection :: String -> [UnitTest] -> TestTree
mkCollection name = TestCollection name . fmap SingleTest

abductionRoot, testDir, benchmarkDir, mipsDir :: FilePath
abductionRoot = "abduction"
testDir       = "test"
benchmarkDir  = "benchmarks"
mipsDir       = benchmarkDir </> "472-mips-pipelined"

allTests :: [TestTree]
allTests =
  [ simple
  , negative
  , mips
  , abduction
  , majorStubs
  , major
  ]

--------------------------------------------------------------------------------
simple :: TestTree
--------------------------------------------------------------------------------
simple = mkCollection "simple" $ ts ++ [t']
  where
    ts      = go <$> names
    t'      = T "stall-hand" "stalling_cpu" $ "examples" </> "verilog" </> "stall.v"
    posDir  = testDir </> "verilog" </> "pos"
    go name = T name "test" $ posDir </> name <.> "v"
    names   =
      [ "tr-test-1"
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


--------------------------------------------------------------------------------
abduction :: TestTree
--------------------------------------------------------------------------------
abduction = mkCollection "abduction" ts
  where
    ts      = go <$> names
    go name = T name "test" $ d </> name <.> "v"
    d       = testDir </> "abduction" </> "pos"
    names   = ["abduction-01"]


--------------------------------------------------------------------------------
mips :: TestTree
--------------------------------------------------------------------------------
mips = TestCollection "mips" [mipsModules, mipsStubs]

mipsModules :: TestTree
mipsModules = mkCollection "modules" $ go <$> names
  where
    go name = T name name $ mipsDir </> name <.> "v"
    names = [ "reg32"
            , "mux3"
            , "control_pipeline"
            , "alu"
            , "alu_ctl"
            , "rom32"
            , "reg_file"
            ]

mipsStubs :: TestTree
mipsStubs = mkCollection "stub" $ go <$> names
  where
    go (ver, name) = T ver "mips_pipeline" $ mipsDir </> name <.> "v"
    names = [ ("v1", "472-mips-fragment")
            , ("v2", "472-mips-fragment-2")
            , ("v3", "472-mips-fragment-3")
            , ("v4", "472-mips-fragment-4")
            ]


--------------------------------------------------------------------------------
negative :: TestTree
--------------------------------------------------------------------------------
negative = mkCollection "negative" $ go <$> names
  where
    negDir = testDir </> "verilog" </> "neg"
    go name = TF name "test" $ negDir </> name <.> "v"
    names = [ "neg-test-1"
            , "neg-test-2"
            , "neg-test-5"
            , "tp"
            , "neg-test-11"
            , "secverilog-neg-01"
            , "secverilog-neg-02"
            ]


--------------------------------------------------------------------------------
majorStubs :: TestTree
--------------------------------------------------------------------------------
majorStubs = mkCollection "major-stub" ts
  where
    b  = benchmarkDir
    d  = b </> "crypto_cores" </> "sha_core" </> "trunk" </> "rtl"
    ts = [ UnitTest { testName    = "sha_stub_3"
                    , moduleName  = "sha256"
                    , verilogFile = d </> "sha256_stub_3.v"
                    , annotFile   = Just $ d </> "annot-sha256_stub_3.json"
                    , testType    = Succ
                    }
         ]


--------------------------------------------------------------------------------
major :: TestTree
--------------------------------------------------------------------------------
major = mkCollection "major" ts
  where
    b  = benchmarkDir
    c  = b </> "crypto_cores"
    ts = [ T  "mips" "mips_pipeline"   $ mipsDir </> "mips_pipeline.v"
         , T  "yarvi" "yarvi"          $ b </> "yarvi" </> "shared" </> "yarvi.v"
         , T  "sha" "sha256"           $ c </> "sha_core" </> "trunk" </> "rtl" </> "sha256.v"
         , T  "fpu" "fpu"              $ b </> "fpu" </> "verilog" </> "fpu.v"
         , TF "fpu-divider" "divider"  $ b </> "fpu2" </> "divider" </> "divider.v"
         , TF "modexp" "ModExp"        $ c </> "RSA4096" </> "ModExp2" </> "ModExp.v"
         , T  "ctalu" "scarv_cop_palu" $ b </> "xcrypto-ref" </> "rtl" </> "coprocessor" </> "scarv_cop_palu.v"
         ]
