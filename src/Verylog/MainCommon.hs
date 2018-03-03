module Verylog.MainCommon where

import Control.Exception  (catch)
import System.IO
import System.Environment (getArgs)
import System.Exit
import System.Console.ANSI

import Verylog.Language.Parser
import Verylog.Language.Types

peHandle :: IRParseError -> IO ()
peHandle e = renderError e >>= hPutStrLn stderr >> exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg) = do
  hSetSGR stderr [ SetColor Foreground Vivid Red
                 , SetConsoleIntensity BoldIntensity
                 ]
  hPutStrLn stderr msg
  hSetSGR stderr []
  exitFailure

getFiles :: IO (FilePath, FilePath)
getFiles = do
  args <- getArgs
  case args of
    [f1,f2] -> return (f1,f2)
    _       -> error "Please run with two files (input & output)"


type PRType = FilePath -> FilePath -> IO ()

runMain              :: PRType -> IO ()
runMain printResults = (getFiles >>= uncurry printResults) `catch` peHandle `catch` passHandle
