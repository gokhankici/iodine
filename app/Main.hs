module Main where

import Control.Exception  (catch)
import Control.Monad
import System.Environment (getArgs)
import System.Exit
import System.IO --(withFile, stderr, hPutStrLn)

import Verylog.HSFGen
import Verylog.Language.Parser
import Verylog.Language.Types

import System.Console.ANSI

main :: IO ()
main =  do
  (getFiles >>= uncurry printResults) `catch` peHandle `catch` passHandle

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  cs <- hsfgen fin
  withFile fout WriteMode $ \h -> do
    let pr = hPutStrLn h
    pr "/* -*- mode: prolog -*- */"
    pr "/* vim: set ft=prolog: */\n" 
    forM_ cs (\c -> pr (pprint c) >> pr "")
  return ()

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

