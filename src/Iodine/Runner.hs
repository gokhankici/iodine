{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Iodine.Runner ( run
                     , main
                     ) where

import           Control.Monad
import qualified Data.ByteString.Lazy             as B
import           Data.Function
import qualified Data.Text                        as T
import           Iodine.IodineArgs
import           Iodine.Language.AnnotationParser
import           Iodine.Language.IRParser
import           Iodine.Pipeline
import           Iodine.Transform.SanityCheck     (SanityCheckError)
import           Iodine.Transform.VCGen           (VCGenError)
import           Iodine.Transform.Query           (QueryError)
import qualified Language.Fixpoint.Solver         as F
import qualified Language.Fixpoint.Types          as FT
import qualified Language.Fixpoint.Types.Config as FC
import           Polysemy                         hiding (run)
import           Polysemy.Error
import           Polysemy.Trace
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process


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
run a = normalizePaths a >>= generateIR >>= checkIR

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
checkIR IodineArgs{..}
  | printIR = do
      irFileContents <- readFile fileName
      putStrLn irFileContents
      result <- parse (fileName, irFileContents)
        & mapError PE
        & errorToIOFinal @E
        & runFinal
      case result of
        Right parsedIR -> forM_ parsedIR print >> return True
        Left e         -> errorHandle e
  | otherwise = do
      irFileContents <- readFile fileName
      annotFileContents <- B.readFile annotFile
      result <- pipeline (T.pack moduleName) (parse (fileName, irFileContents)) (return $ parseAnnotations annotFileContents)
        & mapErrors
        & traceToIO
        & embedToFinal
        & runFinal
      case result of
        Right finfo -> FT.isSafe <$> F.solve config finfo
        Left e      -> errorHandle e
  where
    config = FC.defConfig { FC.eliminate = FC.Some
                          , FC.save      = False
                          , FC.srcFile   = fileName
                          , FC.metadata  = True
                          , FC.minimize  = False
                          }


-- -----------------------------------------------------------------------------
-- Common Functions
-- -----------------------------------------------------------------------------

mapErrors :: Member (Final IO) r
          => Sem (Error IRParseError ':
                  Error SanityCheckError ':
                  Error VCGenError ':
                  Error QueryError ':
                  Error E ':
                  r) a
          -> Sem r (Either E a)
mapErrors act =
  act
  & mapError PE & mapError SE & mapError VE & mapError QE
  & errorToIOFinal @E

data E = PE IRParseError
       | SE SanityCheckError
       | VE VCGenError
       | QE QueryError

errorHandle :: E -> IO Bool
errorHandle (PE e) = renderError e >>= hPutStrLn stderr >> return False
errorHandle (SE e) = hPrint stderr e >> return False
errorHandle (VE e) = hPrint stderr e >> return False
errorHandle (QE e) = hPrint stderr e >> return False