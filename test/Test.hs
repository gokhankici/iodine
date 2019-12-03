{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE StrictData          #-}

module Main (main) where

import qualified Iodine.IodineArgs               as IA
import qualified Iodine.Runner                   as R

import           Control.Exception
import           Control.Lens                    hiding (simple, (<.>))
import           Control.Monad
import           Data.Foldable
import           GHC.Generics                    hiding (moduleName, to)
import           GHC.IO.Handle
import           System.Console.CmdArgs.Explicit
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Spec
import           Text.Printf

data TestArgs =
  TestArgs { _verbose      :: Bool
           , _help         :: Bool
           , _iodineArgs   :: [String]
           , _hspecArgs    :: [String] -- | rest of the positional arguments
           , _runAbduction :: Bool
           , _dryRun       :: Bool
           }
  deriving (Generic, Show)

makeLenses ''TestArgs

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


--------------------------------------------------------------------------------
runTestTree :: TestArgs -> IA.IodineArgs -> TestTree -> Spec
--------------------------------------------------------------------------------
runTestTree ta va = \case
  TestCollection name tests ->
    describe name $ traverse_ (runTestTree ta va) tests

  SingleTest UnitTest{..} ->
    it testName $
    if   ta ^. dryRun
    then printf "iodine %s %s %s\n" verilogFile moduleName af :: IO ()
    else (withSilence $ R.run va') `shouldReturn` (testType == Succ)

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

spec :: TestArgs -> IA.IodineArgs -> Spec
spec ta va = sequential $ traverse_ (runTestTree ta va) allTests

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

silence :: IO a -> IO a
silence action = withFile "/dev/null" AppendMode prepareAndRun
  where
    handles = [stdout, stderr]
    prepareAndRun tmpHandle = go handles
      where
        go [] = action
        go hs = goBracket go tmpHandle hs

    goBracket _ _ [] = error "not possible?"
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

-- -----------------------------------------------------------------------------
-- Argument Parsing
-- -----------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
main :: IO ()
--------------------------------------------------------------------------------
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
