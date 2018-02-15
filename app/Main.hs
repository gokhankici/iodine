module Main where

import Control.Exception  (catch)
import Control.Monad
import System.Environment (getArgs)
import System.Exit
import System.IO (stderr, hPutStrLn)

import Verylog.HSFGen
import Verylog.Language.Parser
import Verylog.Language.Types

main :: IO ()
main =  do
  f <- getSrcFile
  printResults f `catch` peHandle `catch` passHandle
  where
    printResults f = do putStrLn "/* -*- mode: prolog -*- */"
                        putStrLn "/* vim: set ft=prolog: */\n" 
                        cs <- hsfgen f
                        forM_ cs (\c -> putStrLn (pprint c) >> putStrLn "")

peHandle :: IRParseError -> IO ()
peHandle e = renderError e >>= hPutStrLn stderr >> exitFailure

passHandle :: PassError -> IO ()
passHandle (PassError msg) = hPutStrLn stderr msg >> exitFailure

getSrcFile :: IO FilePath
getSrcFile = do
  args <- getArgs
  case args of
    [f] -> return f
    _   -> error "Please run with a single file as input"
